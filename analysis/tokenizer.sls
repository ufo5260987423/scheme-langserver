(library (scheme-langserver analysis tokenizer)
  (export 
    source-file->annotations
    consume-sps-auxiliary)
  (import 
    (chezscheme) 
    (only (srfi :13) string-take string-take-right)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver util io)
    (scheme-langserver analysis bad-brackets-scanner)
    (ufo-try))

;I mainly handle miss-matched () and [], and here's serveral options:
;1st, make a (, ), [ or ] behined or after position
;2nd, just replace position with a space
;3rd, attach a (, ), [ or ]) or ] at the end of source (abandon, though it won't greatly change other tokens' bias, this may cause more faults)
;4th, replace current ) or ] with ] or ).
;I mainly choose 2nd and 4th solution, because it won't change other tokens' bias
;No caso do "unexpected dot", tenho de remover o note para nao alter posicao.

; Helper: extract position from formatted condition-message like "... at char 123 of ..."
(define (private:extract-position-from-message msg)
  (let ([prefix "at char "])
    (let search ([i 0])
      (cond
        [(> i (- (string-length msg) (string-length prefix))) #f]
        [(string=? prefix (substring msg i (+ i (string-length prefix))))
          (let ([start (+ i (string-length prefix))]
              [end (let loop ([j (+ i (string-length prefix))])
                  (if (or (>= j (string-length msg)) (not (char<=? #\0 (string-ref msg j) #\9)))
                    j
                    (loop (+ j 1))))])
            (and (> end start) (string->number (substring msg start end))))]
        [else (search (+ i 1))]))))

; Helper: check if condition-message starts with a given template
(define (private:message-matches? msg template)
  (and (>= (string-length msg) (string-length template))
    (string=? template (substring msg 0 (string-length template)))
    (or (= (string-length msg) (string-length template))
      (char=? #\space (string-ref msg (string-length template))))))

; Helper: safely replace a region with spaces
(define (private:replace-region source position length)
  (let* ([head (if (zero? position) "" (string-take source position))]
      [rest (string-take-right source (max 0 (- (string-length source) position length)))])
    (string-append head (make-string (max 0 length) #\space) rest)))

; Helper: replace multiple positions with spaces, preserving string length
(define (private:replace-positions-with-spaces source positions)
  (let ([result (string-copy source)])
    (for-each
      (lambda (pos)
        (when (and (>= pos 0) (< pos (string-length result)))
          (string-set! result pos #\space)))
      positions)
    result))

; Helper: generate a diagnose for a bad bracket position
(define (private:bad-bracket-position->diagnose source pos)
  (let ([ch (string-ref source pos)])
    (cond
      [(char=? ch #\()
       `(,pos ,(+ pos 1) 1 "Syntax error: unclosed parenthesis" "syntax" "syntax-error")]
      [(char=? ch #\[)
       `(,pos ,(+ pos 1) 1 "Syntax error: unclosed bracket" "syntax" "syntax-error")]
      [(char=? ch #\))
       `(,pos ,(+ pos 1) 1 "Syntax error: unexpected close parenthesis" "syntax" "syntax-error")]
      [(char=? ch #\])
       `(,pos ,(+ pos 1) 1 "Syntax error: unexpected close bracket" "syntax" "syntax-error")]
      [else
       `(,pos ,(+ pos 1) 1 "Syntax error: bad bracket" "syntax" "syntax-error")])))

; Helper: append a diagnose only once per (start . message) key
(define (private:append-diagnose-once ht document diagnose)
  (let ([key (cons (car diagnose) (list-ref diagnose 3))])
    (when (or (not ht) (not (hashtable-contains? ht key)))
      (when ht (hashtable-set! ht key #t))
      (append-new-diagnoses document diagnose))))

; Helper: replace token starting at position up to (but not including) the next delimiter
(define (private:replace-token source position)
  (let ([end (string-find-delimiter source (+ 1 position))])
    (private:replace-region source position (- end position))))

(define (private:compute-error-position condition port)
  (cond
    [(or (private:message-matches? (condition-message condition) "unexpected close parenthesis")
         (private:message-matches? (condition-message condition) "unexpected close bracket")
         (private:message-matches? (condition-message condition) "unexpected dot (.)")
         (private:message-matches? (condition-message condition) "parenthesized list terminated by bracket")
         (private:message-matches? (condition-message condition) "bracketed list terminated by parenthesis"))
     (max 0 (- (port-position port) 1))]
    [else (port-position port)]))

(define (private:tolerant-parse->patch source . maybe-fallback)
  (let ([fallback (if (null? maybe-fallback) 0 (car maybe-fallback))])
  (let loop ([port (open-input-string source)])
    (try 
      (if (eof-object? (get-datum port))
        source
        (loop port))
      (except e
        [(and (condition? e) (pair? (condition-irritants e)) (string? (car (condition-irritants e)))
          (case (car (condition-irritants e))
            [("unexpected dot (.)" "invalid sharp-sign prefix #~c" ) 
              (let* ([position (caddr (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("unexpected close parenthesis" "unexpected close bracket" "unexpected end-of-file reading ~a")
              (let* ([position (caddr (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("parenthesized list terminated by bracket" "bracketed list terminated by parenthesis")
              (let* ([position (- (caddr (condition-irritants e)) 1)]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head " " rest)))]

            ["expected one item after dot (.)" 
              (let* ([position (caddr (condition-irritants e))]
                  [dot-pos 
                    (let search ([i (min position (- (string-length source) 1))])
                      (cond
                        [(< i 0) 0]
                        [(char=? #\. (string-ref source i)) i]
                        [else (search (- i 1))]))])
                (private:tolerant-parse->patch (private:replace-region source dot-pos 1)))]
            ["more than one item found after dot (.)" 
              (let* ([position (caddr (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head " " rest)))]

            ["invalid syntax #!~a" 
              (let* ([position (caddr (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [l 2]
                  [rest (string-take-right source (max 0 (- (string-length source) position l)))])
                (private:tolerant-parse->patch (string-append head (make-string l #\space) rest)))]

            ["invalid boolean #~a~c"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["invalid character name #\\~a" 
              (let* ([position (max 0 (- (caddr (condition-irritants e)) 2))]
                  [what (caadr (condition-irritants e))]
                  [l (+ 2 (string-length what))]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position l)))])
                (private:tolerant-parse->patch (string-append head (make-string l #\space) rest)))]
            ["invalid hex character escape ~a"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["invalid character #\\~a~a~a"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["invalid delimiter ~a for ~a"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["invalid character ~c in string hex escape"
              (let* ([position (caddr (condition-irritants e))]
                  [head (string-take source position)]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            ["invalid string character \\~c"
              (let* ([position (caddr (condition-irritants e))]
                  [esc-start 
                    (if (and (> position 0) (char=? #\\ (string-ref source (- position 1))))
                      (- position 1)
                      position)]
                  [l (if (= esc-start position) 1 2)])
                (private:tolerant-parse->patch (private:replace-region source esc-start l)))]
            ["invalid code point value ~s in string hex escape"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["invalid number syntax ~a"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["cannot represent ~a"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["expected close brace terminating gensym syntax"
              (let* ([position (caddr (condition-irritants e))]
                  [start 
                    (let search ([i (min position (- (string-length source) 1))])
                      (cond
                        [(< i 0) 0]
                        [(char=? #\# (string-ref source i)) i]
                        [else (search (- i 1))]))])
                (private:tolerant-parse->patch (private:replace-region source start (- (string-length source) start))))]

            ["too many vector elements supplied"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["invalid vector length ~s"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["non-fixnum found in fxvector"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["too many fxvector elements supplied"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["invalid fxvector length ~s"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["non-flonum found in flvector"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["too many flvector elements supplied"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["invalid value ~:[~s~;~a~] found in bytevector"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["non-octet found in bytevector"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["mask required for stencil vector"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["not enough stencil vector elements supplied"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["too many stencil vector elements supplied"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["invalid stencil vector mask ~s"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["non-symbol found after #["
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["unrecognized record name ~s"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["too few fields supplied for record ~s"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["too many fields supplied for record ~s"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["duplicate mark #~s= seen"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]
            ["mark #~s= missing"
              (private:tolerant-parse->patch (private:replace-token source (caddr (condition-irritants e))))]

            ["unsupported old fasl format detected---use new format with binary i/o"
              (private:tolerant-parse->patch "")]

            [else (warning 'tokenizer-warning1 "" `(,(condition-who e) ,(condition-message e) ,(condition-irritants e)))
                  source]))]
        [(and (condition? e) (pair? (condition-irritants e)) (pair? (car (condition-irritants e))) (string? (caar (condition-irritants e))))
          (case (caar (condition-irritants e))
            [("unexpected dot (.)" "invalid sharp-sign prefix #~c" ) 
              (let* ([position (caddar (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("unexpected close parenthesis" "unexpected close bracket" "unexpected end-of-file reading ~a")
              (let* ([position (caddar (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("parenthesized list terminated by bracket" "bracketed list terminated by parenthesis")
              (let* ([position (- (caddar (condition-irritants e)) 1)]
                  [head (if (zero? position) "" (string-take source position))]
                  [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                (private:tolerant-parse->patch (string-append head ")" rest)))]
            [else (warning 'tokenizer-warning2 "" `(,(condition-who e) ,(condition-message e) ,(condition-irritants e)))
                  source])]
        ; Handle cases where condition-irritants is #f or contains raw args (no position info)
        [(condition? e)
          (let ([msg (condition-message e)]
                [position (private:extract-position-from-message (condition-message e))])
            (cond
              [(or (private:message-matches? msg "unexpected dot (.)")
                  (private:message-matches? msg "invalid sharp-sign prefix #~c")
                  (private:message-matches? msg "unexpected close parenthesis")
                  (private:message-matches? msg "unexpected close bracket")
                  (private:message-matches? msg "unexpected end-of-file reading ~a"))
                (let* ([position (or position fallback 0)]
                    [head (if (zero? position) "" (string-take source position))]
                    [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                  (private:tolerant-parse->patch (string-append head " " rest)))]
              [(or (private:message-matches? msg "parenthesized list terminated by bracket")
                  (private:message-matches? msg "bracketed list terminated by parenthesis"))
                (let* ([position (if position (- position 1) fallback)]
                    [head (if (zero? position) "" (string-take source position))]
                    [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                  (private:tolerant-parse->patch (string-append head " " rest)))]
              [(or (private:message-matches? msg "expected one item after dot (.)")
                  (private:message-matches? msg "more than one item found after dot (.)"))
                (let* ([position (or position fallback 0)]
                    [dot-pos 
                      (let search ([i (min position (- (string-length source) 1))])
                        (cond
                          [(< i 0) 0]
                          [(char=? #\. (string-ref source i)) i]
                          [else (search (- i 1))]))])
                  (private:tolerant-parse->patch (private:replace-region source dot-pos 1)))]
              [(private:message-matches? msg "invalid syntax #!~a")
                (let* ([position (or position fallback 0)]
                    [head (if (zero? position) "" (string-take source position))]
                    [l 2]
                    [rest (string-take-right source (max 0 (- (string-length source) position l)))])
                  (private:tolerant-parse->patch (string-append head (make-string l #\space) rest)))]
              [(private:message-matches? msg "invalid number syntax ~a")
                (private:tolerant-parse->patch (private:replace-token source (or position fallback 0)))]
              [(private:message-matches? msg "cannot represent ~a")
                (private:tolerant-parse->patch (private:replace-token source (or position fallback 0)))]
              [else (warning 'tokenizer-warning3 "" `(,(condition-who e) ,msg ,(condition-irritants e)))
                source]))]
        [else (warning 'tokenizer-warning4 "" `(,e))
          source])))))

(define (private:condition->diagnose condition source . maybe-fallback)
  (let* ([fallback (if (null? maybe-fallback) 0 (car maybe-fallback))]
         [msg (condition-message condition)]
         [irritants (condition-irritants condition)]
         [position 
           (cond
             [(and (condition? condition) (pair? irritants) (string? (car irritants)) (>= (length irritants) 3) (number? (caddr irritants)))
              (caddr irritants)]
             [(and (condition? condition) (pair? irritants) (pair? (car irritants)) (string? (caar irritants)) (>= (length (car irritants)) 3) (number? (caddar irritants)))
              (caddar irritants)]
             [else (or (private:extract-position-from-message msg) fallback)])]
         [start (max 0 (or position 0))]
         [end 
           (cond
             [(or (private:message-matches? msg "unexpected dot (.)")
                  (private:message-matches? msg "unexpected close parenthesis")
                  (private:message-matches? msg "unexpected close bracket")
                  (private:message-matches? msg "unexpected end-of-file reading ~a")
                  (private:message-matches? msg "parenthesized list terminated by bracket")
                  (private:message-matches? msg "bracketed list terminated by parenthesis")
                  (private:message-matches? msg "more than one item found after dot (.)")
                  (private:message-matches? msg "invalid character ~c in string hex escape"))
              (+ start 1)]
             [(private:message-matches? msg "invalid syntax #!~a")
              (+ start 2)]
             [(private:message-matches? msg "expected one item after dot (.)")
              (let ([dot-pos 
                      (let search ([i (min start (- (string-length source) 1))])
                        (cond [(< i 0) 0] [(char=? #\. (string-ref source i)) i] [else (search (- i 1))]))])
                (+ dot-pos 1))]
             [(private:message-matches? msg "expected close brace terminating gensym syntax")
              (string-length source)]
             [(private:message-matches? msg "invalid string character \\~c")
              (let ([esc-start (if (and (> start 0) (char=? #\\ (string-ref source (- start 1)))) (- start 1) start)])
                (+ esc-start (if (= esc-start start) 1 2)))]
             [(private:message-matches? msg "invalid character name #\\~a")
              (let ([what 
                      (cond
                        [(and (pair? irritants) (pair? (cdr irritants)) (pair? (cadr irritants))) (caadr irritants)]
                        [(and (pair? irritants) (pair? (car irritants)) (pair? (cdar irritants)) (pair? (cadar irritants))) (caadar irritants)]
                        [else ""])])
                (+ start 2 (string-length what)))]
             [(or (private:message-matches? msg "invalid boolean #~a~c")
                  (private:message-matches? msg "invalid hex character escape ~a")
                  (private:message-matches? msg "invalid character #\\~a~a~a")
                  (private:message-matches? msg "invalid delimiter ~a for ~a")
                  (private:message-matches? msg "invalid number syntax ~a")
                  (private:message-matches? msg "cannot represent ~a")
                  (private:message-matches? msg "too many vector elements supplied")
                  (private:message-matches? msg "invalid vector length ~s")
                  (private:message-matches? msg "non-fixnum found in fxvector")
                  (private:message-matches? msg "too many fxvector elements supplied")
                  (private:message-matches? msg "invalid fxvector length ~s")
                  (private:message-matches? msg "non-flonum found in flvector")
                  (private:message-matches? msg "too many flvector elements supplied")
                  (private:message-matches? msg "invalid value ~:[~s~;~a~] found in bytevector")
                  (private:message-matches? msg "non-octet found in bytevector")
                  (private:message-matches? msg "mask required for stencil vector")
                  (private:message-matches? msg "not enough stencil vector elements supplied")
                  (private:message-matches? msg "too many stencil vector elements supplied")
                  (private:message-matches? msg "invalid stencil vector mask ~s")
                  (private:message-matches? msg "non-symbol found after #[")
                  (private:message-matches? msg "unrecognized record name ~s")
                  (private:message-matches? msg "too few fields supplied for record ~s")
                  (private:message-matches? msg "too many fields supplied for record ~s")
                  (private:message-matches? msg "duplicate mark #~s= seen")
                  (private:message-matches? msg "mark #~s= missing")
                  (private:message-matches? msg "invalid code point value ~s in string hex escape"))
              (string-find-delimiter source start)]
             [else (+ start 1)])])
    `(,start ,(min end (string-length source)) 1 ,(string-append "Syntax error: " msg))))

; R7RS-specific fixes. These are only invoked when top-environment is r7rs/s7/goldfish.
(define (private:r7rs-fixable? condition source position)
  (let ([msg (condition-message condition)]
      [irritants (condition-irritants condition)])
    (cond
      [(private:message-matches? msg "invalid sharp-sign prefix #~c")
        (cond
          [(and (pair? irritants) (eqv? #\u (car irritants)))
            (private:r7rs-fix-u8 source position irritants)]
          [(and (pair? irritants) (eqv? #\< (car irritants)))
            (private:s7-fix-bracket-symbol source position)]
          [(and (pair? irritants) (eqv? #\" (car irritants)))
            (or (private:s7-fix-raw-string source position)
                (private:s7-fix-quote-char source position))]
          [(and (pair? irritants) (eqv? #\_ (car irritants)))
            (private:s7-fix-underscore source position)]
          [else
            (private:s7-fix-sharp-symbol source position)])]
      [(private:message-matches? msg "invalid character name #\\~a")
        (private:r7rs-fix-char source position irritants)]
      [else #f])))

(define (private:r7rs-fix-u8 source position irritants)
  (let ([src-len (string-length source)])
    (define (check-at pos)
      (and (>= pos 0)
        (< pos src-len)
        (char=? #\# (string-ref source pos))
        (< (+ pos 1) src-len)
        (char=? #\u (string-ref source (+ pos 1)))
        (< (+ pos 2) src-len)
        (char=? #\8 (string-ref source (+ pos 2)))
        (< (+ pos 3) src-len)
        (char=? #\( (string-ref source (+ pos 3)))))
    (let ([pos (let loop ([p position])
                  (cond
                    [(< p (- position 5)) #f]
                    [(check-at p) p]
                    [else (loop (- p 1))]))])
      (if pos
        (let ([head (string-take source (+ pos 1))]
            [rest (string-take-right source (- src-len (+ pos 3)))])
          (string-append head "vu8" rest))
        #f))))

(define (private:r7rs-fix-char source position irritants)
  (let* ([name
            (cond
              [(and (pair? irritants) (string? (car irritants)))
                (car irritants)]
              [(and (pair? irritants) (pair? (car irritants)) (string? (caar irritants)))
                (caar irritants)]
              [else #f])]
      [replacement
        (case name
          [("null") "nul"]
          [("escape") "esc"]
          [else #f])])
    (if replacement
      (let* ([src-len (string-length source)]
          [old-name-len (string-length name)]
          [old-len (+ 2 old-name-len)]
          [search-str (string-append "#\\" name)]
          [found (let loop ([pos (max 0 (- position old-len))])
                   (cond
                     [(>= pos src-len) #f]
                     [(and (<= (+ pos old-len) src-len)
                           (string=? search-str (substring source pos (+ pos old-len))))
                       pos]
                     [else (loop (+ pos 1))]))])
        (if found
          (let ([head (string-take source found)]
              [rest (string-take-right source (- src-len (+ found old-len)))])
            (string-append head (string-append "#\\" replacement) rest))
          #f))
      #f)))

(define (private:s7-fix-underscore source position)
  (let ([src-len (string-length source)])
    (define (identifier-char? c)
      (or (char<=? #\a c #\z)
        (char<=? #\A c #\Z)
        (char<=? #\0 c #\9)
        (memv c '(#\- #\? #\! #\* #\+ #\. #\/ #\: #\< #\= #\> #\@ #\^ #\~))))
    (define (read-identifier pos)
      (let loop ([p pos] [chars '()])
        (if (and (< p src-len) (identifier-char? (string-ref source p)))
          (loop (+ p 1) (cons (string-ref source p) chars))
          (list->string (reverse chars)))))
    (define (check-at pos)
      (and (>= pos 0)
        (< pos src-len)
        (char=? #\# (string-ref source pos))
        (< (+ pos 1) src-len)
        (char=? #\_ (string-ref source (+ pos 1)))))
    (let ([pos (let loop ([p position])
                 (cond
                   [(< p (- position 5)) #f]
                   [(check-at p) p]
                   [else (loop (- p 1))]))])
      (if pos
        (let ([id (read-identifier (+ pos 2))])
          (if (string=? id "")
            #f
            (let* ([head (string-take source pos)]
                [id-len (string-length id)]
                [rest (string-take-right source (- src-len (+ pos 2 id-len)))])
              (string-append head id rest))))
        #f))))

(define (private:s7-fix-bracket-symbol source position)
  (let ([src-len (string-length source)])
    (define (find-open pos)
      (and (>= pos 0)
        (< pos src-len)
        (char=? #\# (string-ref source pos))
        (< (+ pos 1) src-len)
        (char=? #\< (string-ref source (+ pos 1)))))
    (define (find-close pos)
      (let loop ([p pos])
        (cond
          [(>= p src-len) #f]
          [(char=? #\> (string-ref source p)) p]
          [else (loop (+ p 1))])))
    (let ([open-pos (let loop ([p position])
                      (cond
                        [(< p (- position 50)) #f]
                        [(find-open p) p]
                        [else (loop (- p 1))]))])
      (if open-pos
        (let ([close-pos (find-close (+ open-pos 2))])
          (if close-pos
            (let ([head (string-take source open-pos)]
                [body (substring source open-pos (+ close-pos 1))]
                [rest (string-take-right source (- src-len close-pos 1))])
              (string-append head "|" body "|" rest))
            #f))
        #f))))

(define (private:s7-fix-raw-string source position)
  (let ([src-len (string-length source)])
    (define (find-prefix pos)
      (and (>= pos 0)
        (< pos src-len)
        (char=? #\# (string-ref source pos))
        (< (+ pos 1) src-len)
        (char=? #\" (string-ref source (+ pos 1)))))
    (define (read-delimiter pos)
      (let loop ([p pos] [chars '()])
        (cond
          [(>= p src-len) (values #f '())]
          [(char=? #\" (string-ref source p)) (values p (reverse chars))]
          [else (loop (+ p 1) (cons (string-ref source p) chars))])))
    (define (find-suffix start delimiter)
      (let ([del-len (length delimiter)])
        (let loop ([p start])
          (cond
            [(>= p src-len) #f]
            [(and (char=? #\" (string-ref source p))
                (let check ([i 0])
                  (cond
                    [(= i del-len)
                      (and (< (+ p del-len 1) src-len)
                        (char=? #\" (string-ref source (+ p del-len 1)))
                        (+ p del-len 1))]
                    [(>= (+ p 1 i) src-len) #f]
                    [(char=? (list-ref delimiter i) (string-ref source (+ p 1 i)))
                      (check (+ i 1))]
                    [else #f])))
             => (lambda (end) end)]
            [else (loop (+ p 1))]))))
    (let ([open-pos (let loop ([p position])
                      (cond
                        [(< p (- position 20)) #f]
                        [(find-prefix p) p]
                        [else (loop (- p 1))]))])
      (if open-pos
        (let-values ([(del-end delimiter) (read-delimiter (+ open-pos 2))])
          (if del-end
            (let ([body-start (+ del-end 1)])
              (let ([close-pos (find-suffix body-start delimiter)])
                (if close-pos
                  (let ([head (string-take source open-pos)]
                      [rest (string-take-right source (- src-len close-pos 1))])
                    (string-append head "\"\"" rest))
                  #f)))
            #f))
        #f))))

(define (private:s7-fix-quote-char source position)
  (let ([src-len (string-length source)])
    (define (delimiter? c)
      (or (char=? c #\space)
          (char=? c #\newline)
          (char=? c #\return)
          (char=? c #\tab)
          (char=? c #\()
          (char=? c #\))
          (char=? c #\[)
          (char=? c #\])
          (char=? c #\;)))
    (define (check-at pos)
      (and (>= pos 0)
        (< pos src-len)
        (char=? #\# (string-ref source pos))
        (< (+ pos 1) src-len)
        (char=? #\" (string-ref source (+ pos 1)))
        (or (>= (+ pos 2) src-len)
            (delimiter? (string-ref source (+ pos 2))))))
    (let ([pos (let loop ([p position])
                 (cond
                   [(< p (- position 5)) #f]
                   [(check-at p) p]
                   [else (loop (- p 1))]))])
      (if pos
        (let ([head (string-take source pos)]
            [rest (string-take-right source (- src-len (+ pos 2)))])
          (string-append head "#\\x22" rest))
        #f))))

(define (private:s7-fix-sharp-symbol source position)
  (let ([src-len (string-length source)])
    (define (token-char? c)
      (or (char<=? #\a c #\z)
        (char<=? #\A c #\Z)
        (char<=? #\0 c #\9)
        (memv c '(#\- #\? #\! #\* #\+ #\. #\/ #\: #\< #\= #\> #\@ #\^ #\~ #\#))))
    (define (read-token pos)
      (let loop ([p pos] [chars '()])
        (if (and (< p src-len)
              (or (token-char? (string-ref source p))
                (and (= p (+ pos 1)) (char=? #\tab (string-ref source p)))))
          (loop (+ p 1) (cons (string-ref source p) chars))
          (list->string (reverse chars)))))
    (define (check-at pos)
      (and (>= pos 0)
        (< pos src-len)
        (char=? #\# (string-ref source pos))))
    (let ([pos (let loop ([p position])
                 (cond
                   [(< p (- position 20)) #f]
                   [(check-at p) p]
                   [else (loop (- p 1))]))])
      (if pos
        (let ([token (read-token pos)])
          (if (or (string=? token "#") (string=? token ""))
            #f
            (let* ([head (string-take source pos)]
                [token-len (string-length token)]
                [rest (string-take-right source (- src-len (+ pos token-len)))])
              (string-append head "|" token "|" rest))))
        #f))))

(define source-file->annotations
  (case-lambda
    ([path] (source-file->annotations (read-string path) path))
    ([source path] (source-file->annotations source path (consume-sps-auxiliary source) #t #f 'r6rs))
    ([source path start-position] (source-file->annotations source path start-position #t #f 'r6rs))
    ([source path start-position tolerant?]
      (source-file->annotations source path start-position tolerant? #f 'r6rs))
    ([source path start-position tolerant? maybe-document]
      (source-file->annotations source path start-position tolerant? maybe-document 'r6rs))
    ([source path start-position tolerant? maybe-document top-environment]
      (if (file-exists? path)
        (let* ([preprocessed-source
                 (if tolerant?
                   (let* ([bad-positions (compute-bad-brackets source)]
                          [cleaned-source (private:replace-positions-with-spaces source bad-positions)]
                          [seen-ht (if maybe-document (make-hashtable equal-hash equal?) #f)])
                     (when maybe-document
                       (for-each
                         (lambda (pos)
                           (private:append-diagnose-once seen-ht maybe-document
                             (private:bad-bracket-position->diagnose source pos)))
                         bad-positions))
                     cleaned-source)
                   source)]
               [port (open-string-input-port preprocessed-source)]
               [source-file-descriptor (make-source-file-descriptor path (open-file-input-port path))])
          (set-port-position! port start-position)
          (filter annotation? 
            (let loop ([position start-position])
              (try
                (let-values ([(ann end-pos) (get-datum/annotations port source-file-descriptor position)]) 
                  (if (= position (port-position port))
                    '()
                    `(,ann . ,(loop (port-position port)))))
                (except e
                  [(and tolerant? (condition? e))
                    (let ([error-position (private:compute-error-position e port)])
                      (cond
                        [(and (memq top-environment '(r7rs s7 goldfish))
                              (private:r7rs-fixable? e source error-position))
                         => (lambda (patched-source)
                              (source-file->annotations patched-source path start-position tolerant? maybe-document top-environment))]
                        [else
                          (when maybe-document
                            (append-new-diagnoses maybe-document (append (private:condition->diagnose e preprocessed-source error-position) '("syntax" "syntax-error"))))
                          (let ([after (private:tolerant-parse->patch preprocessed-source error-position)])
                            (if (= (string-length after) (string-length preprocessed-source))
                              (source-file->annotations after path start-position #f maybe-document top-environment)
                              (error 'tokenizer-error (condition-message e) (condition-irritants e))))]))]
                  [(condition? e)
                    (let ([error-position (private:compute-error-position e port)])
                      (when maybe-document
                        (append-new-diagnoses maybe-document (append (private:condition->diagnose e preprocessed-source error-position) '("syntax" "syntax-error"))))
                      (error 'tokenizer-error0 path `(,preprocessed-source ,path ,error-position ,tolerant? ,(condition-who e) ,(condition-message e) ,(condition-irritants e))))]
                  [else 
                    (let ([error-position (max 0 (- (port-position port) 1))])
                      (when maybe-document
                        (append-new-diagnoses maybe-document `(,error-position ,(+ error-position 1) 1 "Syntax error: unknown parse error" "syntax" "syntax-error")))
                      (warning 'tokenizer-error0 path `(,preprocessed-source ,path ,error-position ,tolerant?))
                      '())])))))
          (begin
            (when maybe-document
              (append-new-diagnoses maybe-document `(0 0 1 ,(string-append "File not found: " path) "syntax" "file-not-found")))
            (warning 'no-such-file-warning path '())
            '())))))

;https://github.com/cisco/ChezScheme/blob/e63e5af1a5d6805c96fa8977e7bd54b3b516cff6/s/7.ss#L268-L280
; consume
; #!/usr/bin/env scheme-script
; #!r6rs
; #!...
; line comment: ; ... 
; don't need consume datum comment  
(define (consume-sps-auxiliary source)
  (let* ([ip (open-string-input-port source)])
    (let loop ([c (get-char ip)]
        [inline-comment? #f])
      (cond 
        [(eof-object? c) (- (port-position ip) 1)]
        [(eqv? #\newline c) (loop (get-char ip) #f)]
        [(eqv? #\return c) (loop (get-char ip) #f)]
        [(eqv? c #\;
        ) (loop (get-char ip) #t)]
        [(eqv? c #\#) 
          (cond
            [(and (not inline-comment?) (eqv? #\| (lookahead-char ip)))
              (get-char ip)
              (consume-block-comment ip)
              (loop (get-char ip) #f)]
            [(and (not inline-comment?) (eqv? #\; (lookahead-char ip)))
              (get-char ip)
              (guard (e [else (void)])
                (get-datum ip))
              (loop (get-char ip) #f)]
            [else (loop (get-char ip) inline-comment?)])]
        [(and (not inline-comment?) (eqv? c #\( )) (- (port-position ip) 1)]
        [else (loop (get-char ip) inline-comment?)]))))

; block comment: #| ... |#
; may be nested
(define (consume-block-comment char-input-port)
  (let loop ([c (get-char char-input-port)])
    (cond
      [(and (eqv? c #\|) (eqv? (lookahead-char char-input-port) #\#))
        (get-char char-input-port) 
        (port-position char-input-port)]
      [(and (eqv? c #\#) (eqv? (lookahead-char char-input-port) #\|))
        (get-char char-input-port) 
        (consume-block-comment char-input-port)]
      [(eof-object? c) (port-position char-input-port)]
      [else (loop (get-char char-input-port))])))

(define (string-find-delimiter s position)
  (cond
    [(>= position (string-length s)) (string-length s)]
    [else
      (case (string-ref s position)
        [#\( position]
        [#\) position]
        [#\[ position]
        [#\] position]
        [#\" position];"
        [#\; position]
        [#\# position]
        [#\space position]
        [#\newline position]
        [#\linefeed position]
        [#\tab position]
        [#\return position]
        [else (string-find-delimiter s (+ 1 position))])]))
)
