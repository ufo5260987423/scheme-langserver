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

; Helper: find next delimiter position in string
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

(define private:tolerant-parse->patch
  (case-lambda
    ([source] (private:tolerant-parse->patch source #f 0))
    ([source maybe-document] (private:tolerant-parse->patch source maybe-document 0))
    ([source maybe-document fallback]
      (let ([original-source source]
            [seen-ht (if maybe-document (make-hashtable equal-hash equal?) #f)])
        (let inner ([source source])
          (let loop ([port (open-input-string source)])
            (try 
              (if (eof-object? (get-datum port))
                source
                (loop port))
              (except e
                [(condition? e)
                  (let* ([irritants (condition-irritants e)]
                      [msg (condition-message e)]
                      [template
                        (cond
                          [(and (pair? irritants) (string? (car irritants))) (car irritants)]
                          [(and (pair? irritants) (pair? (car irritants)) (string? (caar irritants))) (caar irritants)]
                          [(private:message-matches? msg "unexpected close parenthesis") "unexpected close parenthesis"]
                          [(private:message-matches? msg "unexpected close bracket") "unexpected close bracket"]
                          [(private:message-matches? msg "parenthesized list terminated by bracket") "parenthesized list terminated by bracket"]
                          [(private:message-matches? msg "bracketed list terminated by parenthesis") "bracketed list terminated by parenthesis"]
                          [(private:message-matches? msg "unexpected end-of-file reading ~a") "unexpected end-of-file reading ~a"]
                          [(private:message-matches? msg "unexpected dot (.)") "unexpected dot (.)"]
                          [(private:message-matches? msg "invalid sharp-sign prefix #~c") "invalid sharp-sign prefix #~c"]
                          [(private:message-matches? msg "expected one item after dot (.)") "expected one item after dot (.)"]
                          [(private:message-matches? msg "more than one item found after dot (.)") "more than one item found after dot (.)"]
                          [(private:message-matches? msg "invalid syntax #!~a") "invalid syntax #!~a"]
                          [(private:message-matches? msg "invalid number syntax ~a") "invalid number syntax ~a"]
                          [(private:message-matches? msg "cannot represent ~a") "cannot represent ~a"]
                          [else #f])]
                      [position
                        (cond
                          [(and (pair? irritants) (string? (car irritants)) (>= (length irritants) 3) (number? (caddr irritants))) (caddr irritants)]
                          [(and (pair? irritants) (pair? (car irritants)) (>= (length (car irritants)) 3) (number? (caddar irritants))) (caddar irritants)]
                          [else (or (private:extract-position-from-message msg) fallback)])]
                      [what
                        (cond
                          [(and (pair? irritants) (pair? (cdr irritants)) (pair? (cadr irritants))) (caadr irritants)]
                          [(and (pair? irritants) (pair? (car irritants)) (pair? (cdar irritants)) (pair? (cadar irritants))) (caadar irritants)]
                          [else ""])])
                    (when maybe-document
                      (case template
                        [("unexpected end-of-file reading ~a" "unexpected dot (.)" "invalid sharp-sign prefix #~c" ) 
                          (void)]
                        [("parenthesized list terminated by bracket" "bracketed list terminated by parenthesis")
                          (private:append-diagnose-once seen-ht maybe-document (append (private:condition->diagnose e original-source fallback) '("syntax" "syntax-error")))]
                        [else
                          (private:append-diagnose-once seen-ht maybe-document (append (private:condition->diagnose e original-source fallback) '("syntax" "syntax-error")))]))
                    (case template
                    ;; Group 1: Parenthesis / bracket
                    [("unexpected close parenthesis" "unexpected close bracket")
                      (let* ([head (if (zero? position) "" (string-take source position))]
                          [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                        (inner (string-append head " " rest)))]
                    [("parenthesized list terminated by bracket" "bracketed list terminated by parenthesis")
                      (let* ([position (- position 1)]
                          [head (if (zero? position) "" (string-take source position))]
                          [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                        (inner (string-append head " " rest)))]

                    ;; Group 2: EOF / dot / sharp-sign
                    [("unexpected end-of-file reading ~a" "unexpected dot (.)" "invalid sharp-sign prefix #~c" ) 
                      (let* ([head (if (zero? position) "" (string-take source position))]
                          [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                        (inner (string-append head " " rest)))]
                    ["expected one item after dot (.)" 
                      (let* ([dot-pos 
                            (let search ([i (min position (- (string-length source) 1))])
                              (cond
                                [(< i 0) 0]
                                [(char=? #\. (string-ref source i)) i]
                                [else (search (- i 1))]))])
                        (inner (private:replace-region source dot-pos 1)))]
                    ["more than one item found after dot (.)" 
                      (let* ([head (if (zero? position) "" (string-take source position))]
                          [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                        (inner (string-append head " " rest)))]

                    ;; Group 3: Vector family
                    ["too many vector elements supplied"
                      (inner (private:replace-token source position))]
                    ["invalid vector length ~s"
                      (inner (private:replace-token source position))]
                    ["non-fixnum found in fxvector"
                      (inner (private:replace-token source position))]
                    ["too many fxvector elements supplied"
                      (inner (private:replace-token source position))]
                    ["invalid fxvector length ~s"
                      (inner (private:replace-token source position))]
                    ["non-flonum found in flvector"
                      (inner (private:replace-token source position))]
                    ["too many flvector elements supplied"
                      (inner (private:replace-token source position))]
                    ["invalid value ~:[~s~;~a~] found in bytevector"
                      (inner (private:replace-token source position))]
                    ["non-octet found in bytevector"
                      (inner (private:replace-token source position))]
                    ["mask required for stencil vector"
                      (inner (private:replace-token source position))]
                    ["not enough stencil vector elements supplied"
                      (inner (private:replace-token source position))]
                    ["too many stencil vector elements supplied"
                      (inner (private:replace-token source position))]
                    ["invalid stencil vector mask ~s"
                      (inner (private:replace-token source position))]

                    ;; Group 4: Atom / literal
                    ["invalid syntax #!~a" 
                      (let* ([head (if (zero? position) "" (string-take source position))]
                          [l 2]
                          [rest (string-take-right source (max 0 (- (string-length source) position l)))])
                        (inner (string-append head (make-string l #\space) rest)))]
                    ["invalid boolean #~a~c"
                      (inner (private:replace-token source position))]
                    ["invalid character name #\\~a" 
                      (let* ([position (max 0 (- position 2))]
                          [l (+ 2 (string-length what))]
                          [head (if (zero? position) "" (string-take source position))]
                          [rest (string-take-right source (max 0 (- (string-length source) position l)))])
                        (inner (string-append head (make-string l #\space) rest)))]
                    ["invalid hex character escape ~a"
                      (inner (private:replace-token source position))]
                    ["invalid character #\\~a~a~a"
                      (inner (private:replace-token source position))]
                    ["invalid delimiter ~a for ~a"
                      (inner (private:replace-token source position))]
                    ["invalid character ~c in string hex escape"
                      (let* ([head (string-take source position)]
                          [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                        (inner (string-append head " " rest)))]
                    ["invalid string character \\~c"
                      (let* ([esc-start 
                            (if (and (> position 0) (char=? #\\ (string-ref source (- position 1))))
                              (- position 1)
                              position)]
                          [l (if (= esc-start position) 1 2)])
                        (inner (private:replace-region source esc-start l)))]
                    ["invalid code point value ~s in string hex escape"
                      (inner (private:replace-token source position))]
                    ["invalid number syntax ~a"
                      (inner (private:replace-token source position))]
                    ["cannot represent ~a"
                      (inner (private:replace-token source position))]

                    ;; Group 5: Gensym / record / graph-mark
                    ["expected close brace terminating gensym syntax"
                      (let* ([start 
                            (let search ([i (min position (- (string-length source) 1))])
                              (cond
                                [(< i 0) 0]
                                [(char=? #\# (string-ref source i)) i]
                                [else (search (- i 1))]))])
                        (inner (private:replace-region source start (- (string-length source) start))))]
                    ["non-symbol found after #["
                      (inner (private:replace-token source position))]
                    ["unrecognized record name ~s"
                      (inner (private:replace-token source position))]
                    ["too few fields supplied for record ~s"
                      (inner (private:replace-token source position))]
                    ["too many fields supplied for record ~s"
                      (inner (private:replace-token source position))]
                    ["duplicate mark #~s= seen"
                      (inner (private:replace-token source position))]
                    ["mark #~s= missing"
                      (inner (private:replace-token source position))]

                    ;; Group 6: Fasl
                    ["unsupported old fasl format detected---use new format with binary i/o"
                      (inner "")]

                    [else
                      (warning 'tokenizer-warning "" `(,(condition-who e) ,msg ,irritants))
                      source]))]
              [else
                (warning 'tokenizer-warning4 "" `(,e))
                source]))))))))

(define (private:condition->diagnose condition source . maybe-fallback)
  (let* ([fallback (if (null? maybe-fallback) 0 (car maybe-fallback))]
         [msg (condition-message condition)]
         [irritants (condition-irritants condition)]
         [actual-msg
           (if (string=? msg "~? at char ~a of ~s")
             (cond
               [(and (pair? irritants) (string? (car irritants))) (car irritants)]
               [(and (pair? irritants) (pair? (car irritants)) (string? (caar irritants))) (caar irritants)]
               [else msg])
             msg)]
         [position 
           (cond
             [(and (condition? condition) (pair? irritants) (string? (car irritants)) (>= (length irritants) 3) (number? (caddr irritants)))
              (caddr irritants)]
             [(and (condition? condition) (pair? irritants) (pair? (car irritants)) (string? (caar irritants)) (>= (length (car irritants)) 3) (number? (caddar irritants)))
              (caddar irritants)]
             [else (or (private:extract-position-from-message msg) fallback)])]
         [start 
           (let ([raw-start (max 0 (or position 0))])
             (if (or (private:message-matches? actual-msg "parenthesized list terminated by bracket")
                     (private:message-matches? actual-msg "bracketed list terminated by parenthesis"))
               (max 0 (- raw-start 1))
               raw-start))]
         [end 
           (cond
             [(or (private:message-matches? actual-msg "unexpected dot (.)")
                  (private:message-matches? actual-msg "unexpected close parenthesis")
                  (private:message-matches? actual-msg "unexpected close bracket")
                  (private:message-matches? actual-msg "unexpected end-of-file reading ~a")
                  (private:message-matches? actual-msg "parenthesized list terminated by bracket")
                  (private:message-matches? actual-msg "bracketed list terminated by parenthesis")
                  (private:message-matches? actual-msg "more than one item found after dot (.)")
                  (private:message-matches? actual-msg "invalid character ~c in string hex escape"))
              (+ start 1)]
             [(private:message-matches? actual-msg "invalid syntax #!~a")
              (+ start 2)]
             [(private:message-matches? actual-msg "expected one item after dot (.)")
              (let ([dot-pos 
                      (let search ([i (min start (- (string-length source) 1))])
                        (cond [(< i 0) 0] [(char=? #\. (string-ref source i)) i] [else (search (- i 1))]))])
                (+ dot-pos 1))]
             [(private:message-matches? actual-msg "expected close brace terminating gensym syntax")
              (string-length source)]
             [(private:message-matches? actual-msg "invalid string character \\~c")
              (let ([esc-start (if (and (> start 0) (char=? #\\ (string-ref source (- start 1)))) (- start 1) start)])
                (+ esc-start (if (= esc-start start) 1 2)))]
             [(private:message-matches? actual-msg "invalid character name #\\~a")
              (let ([what 
                      (cond
                        [(and (pair? irritants) (pair? (cdr irritants)) (pair? (cadr irritants))) (caadr irritants)]
                        [(and (pair? irritants) (pair? (car irritants)) (pair? (cdar irritants)) (pair? (cadar irritants))) (caadar irritants)]
                        [else ""])])
                (+ start 2 (string-length what)))]
             [(or (private:message-matches? actual-msg "invalid boolean #~a~c")
                  (private:message-matches? actual-msg "invalid hex character escape ~a")
                  (private:message-matches? actual-msg "invalid character #\\~a~a~a")
                  (private:message-matches? actual-msg "invalid delimiter ~a for ~a")
                  (private:message-matches? actual-msg "invalid number syntax ~a")
                  (private:message-matches? actual-msg "cannot represent ~a")
                  (private:message-matches? actual-msg "too many vector elements supplied")
                  (private:message-matches? actual-msg "invalid vector length ~s")
                  (private:message-matches? actual-msg "non-fixnum found in fxvector")
                  (private:message-matches? actual-msg "too many fxvector elements supplied")
                  (private:message-matches? actual-msg "invalid fxvector length ~s")
                  (private:message-matches? actual-msg "non-flonum found in flvector")
                  (private:message-matches? actual-msg "too many flvector elements supplied")
                  (private:message-matches? actual-msg "invalid value ~:[~s~;~a~] found in bytevector")
                  (private:message-matches? actual-msg "non-octet found in bytevector")
                  (private:message-matches? actual-msg "mask required for stencil vector")
                  (private:message-matches? actual-msg "not enough stencil vector elements supplied")
                  (private:message-matches? actual-msg "too many stencil vector elements supplied")
                  (private:message-matches? actual-msg "invalid stencil vector mask ~s")
                  (private:message-matches? actual-msg "non-symbol found after #[")
                  (private:message-matches? actual-msg "unrecognized record name ~s")
                  (private:message-matches? actual-msg "too few fields supplied for record ~s")
                  (private:message-matches? actual-msg "too many fields supplied for record ~s")
                  (private:message-matches? actual-msg "duplicate mark #~s= seen")
                  (private:message-matches? actual-msg "mark #~s= missing")
                  (private:message-matches? actual-msg "invalid code point value ~s in string hex escape"))
              (string-find-delimiter source start)]
             [else (+ start 1)])])
    `(,start ,(min end (string-length source)) 1 ,(string-append "Syntax error: " actual-msg))))

(define (private:append-diagnose-once ht document diagnose)
  (let ([key (cons (car diagnose) (list-ref diagnose 3))])
    (when (or (not ht) (not (hashtable-contains? ht key)))
      (when ht (hashtable-set! ht key #t))
      (append-new-diagnoses document diagnose))))

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
            [else (loop (get-char ip) inline-comment?)])]
        [(and (not inline-comment?) (eqv? c #\( )) (- (port-position ip) 1)]
        [else (loop (get-char ip) inline-comment?)]))))

(define source-file->annotations
  (case-lambda
    ([path] (source-file->annotations (read-string path) path))
    ([source path] (source-file->annotations source path (consume-sps-auxiliary source) #t))
    ([source path start-position] (source-file->annotations source path start-position #t))
    ([source path start-position tolerant?]
      (source-file->annotations source path start-position tolerant? #f))
    ([source path start-position tolerant? maybe-document]
      (if (file-exists? path)
        (let ([source-file-descriptor (make-source-file-descriptor path (open-file-input-port path))])
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
                 [port (open-string-input-port preprocessed-source)])
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
                      (let ([after (private:tolerant-parse->patch preprocessed-source maybe-document (private:compute-error-position e port))])
                        (if (= (string-length after) (string-length preprocessed-source))
                          (source-file->annotations after path start-position #t maybe-document)
                          (error 'tokenizer-error (condition-message e) (condition-irritants e))))]
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
                        '())]))))))
          (begin
            (when maybe-document
              (append-new-diagnoses maybe-document `(0 0 1 ,(string-append "File not found: " path) "syntax" "file-not-found")))
            (warning 'no-such-file-warning path '())
            '()))))))

;https://github.com/cisco/ChezScheme/blob/e63e5af1a5d6805c96fa8977e7bd54b3b516cff6/s/7.ss#L268-L280
; consume
; #!/usr/bin/env scheme-script
; #!r6rs
; #!...

