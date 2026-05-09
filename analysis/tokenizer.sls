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

; Helper: replace token starting at position up to (but not including) the next delimiter
(define (private:replace-token source position)
  (let ([end (string-find-delimiter source (+ 1 position))])
    (private:replace-region source position (- end position))))

(define (private:tolerant-parse->patch source)
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
                (let* ([position (or position 0)]
                    [head (if (zero? position) "" (string-take source position))]
                    [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                  (private:tolerant-parse->patch (string-append head " " rest)))]
              [(or (private:message-matches? msg "parenthesized list terminated by bracket")
                  (private:message-matches? msg "bracketed list terminated by parenthesis"))
                (let* ([position (if position (- position 1) 0)]
                    [head (if (zero? position) "" (string-take source position))]
                    [rest (string-take-right source (max 0 (- (string-length source) position 1)))])
                  (private:tolerant-parse->patch (string-append head " " rest)))]
              [(or (private:message-matches? msg "expected one item after dot (.)")
                  (private:message-matches? msg "more than one item found after dot (.)"))
                (let* ([position (or position 0)]
                    [dot-pos 
                      (let search ([i (min position (- (string-length source) 1))])
                        (cond
                          [(< i 0) 0]
                          [(char=? #\. (string-ref source i)) i]
                          [else (search (- i 1))]))])
                  (private:tolerant-parse->patch (private:replace-region source dot-pos 1)))]
              [(private:message-matches? msg "invalid syntax #!~a")
                (let* ([position (or position 0)]
                    [head (if (zero? position) "" (string-take source position))]
                    [l 2]
                    [rest (string-take-right source (max 0 (- (string-length source) position l)))])
                  (private:tolerant-parse->patch (string-append head (make-string l #\space) rest)))]
              [(private:message-matches? msg "invalid number syntax ~a")
                (private:tolerant-parse->patch (private:replace-token source (or position 0)))]
              [(private:message-matches? msg "cannot represent ~a")
                (private:tolerant-parse->patch (private:replace-token source (or position 0)))]
              [else (warning 'tokenizer-warning3 "" `(,(condition-who e) ,msg ,(condition-irritants e)))
                source]))]
        [else (warning 'tokenizer-warning4 "" `(,e))
          source]))))

(define (private:condition->diagnose condition source)
  (let* ([msg (condition-message condition)]
         [irritants (condition-irritants condition)]
         [position 
           (cond
             [(and (condition? condition) (pair? irritants) (string? (car irritants)) (>= (length irritants) 3) (number? (caddr irritants)))
              (caddr irritants)]
             [(and (condition? condition) (pair? irritants) (pair? (car irritants)) (string? (caar irritants)) (>= (length (car irritants)) 3) (number? (caddar irritants)))
              (caddar irritants)]
             [else (or (private:extract-position-from-message msg) 0)])]
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

(define source-file->annotations
  (case-lambda
    ([path] (source-file->annotations (read-string path) path))
    ([source path] (source-file->annotations source path (consume-sps-auxiliary source) #t))
    ([source path start-position] (source-file->annotations source path start-position #t))
    ([source path start-position tolerant?]
      (source-file->annotations source path start-position tolerant? #f))
    ([source path start-position tolerant? maybe-document]
      (if (file-exists? path)
        (let ([port (open-string-input-port source)]
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
                    (when maybe-document
                      (append-new-diagnoses maybe-document (private:condition->diagnose e source)))
                    (let ([after (private:tolerant-parse->patch source)])
                      (if (= (string-length after) (string-length source))
                        (source-file->annotations after path start-position #f maybe-document)
                        (error 'tokenizer-error (condition-message e) (condition-irritants e))))]
                  [(condition? e)
                    (when maybe-document
                      (append-new-diagnoses maybe-document (private:condition->diagnose e source)))
                    (error 'tokenizer-error0 path `(,source ,path ,position ,tolerant? ,(condition-who e) ,(condition-message e) ,(condition-irritants e)))]
                  [else (warning 'tokenizer-error0 path `(,source ,path ,position ,tolerant?))
                    '()])))))
          (warning 'no-such-file-warning path '())))))

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
