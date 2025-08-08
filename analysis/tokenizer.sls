(library (scheme-langserver analysis tokenizer)
  (export 
    source-file->annotations)
  (import 
    (chezscheme) 
    (only (srfi :13) string-take string-take-right)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver util io)
    (ufo-try))

;I mainly handle miss-matched () and [], and here's serveral options:
;1st, make a (, ), [ or ] behined or after position
;2nd, just replace position with a space
;3rd, attach a (, ), [ or ]) or ] at the end of source (abandon, though it won't greatly change other tokens' bias, this may cause more faults)
;4th, replace current ) or ] with ] or ).
;I mainly choose 2nd and 4th solution, because it won't change other tokens' bias
;No caso do "unexpected dot", tenho de remover o note para nao alter posicao.
(define (private:tolerant-parse->patch source)
  (let loop ([port (open-input-string source)])
    (try 
      (if (eof-object? (get-datum port))
        source
        (loop port))
      (except e
        [(and (condition? e) (string? (car (condition-irritants e)))
          (case (car (condition-irritants e))
            [("unexpected dot (.)" "invalid sharp-sign prefix #~c" ) 
              (let* ([position (caddr (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [what (vector-ref (list->vector (string->list source)) position)]
                  [rest (string-take-right source (- (string-length source) position 1))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("unexpected close parenthesis" "unexpected close bracket" "unexpected end-of-file reading ~a")
              (let* ([position (caddr (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [what (vector-ref (list->vector (string->list source)) position)]
                  [rest (string-take-right source (- (string-length source) position 1))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("parenthesized list terminated by bracket" "bracketed list terminated by parenthesis")
              (let* ([position (- (caddr (condition-irritants e)) 1)]
                  [head (if (zero? position) "" (string-take source position))]
                  [what (vector-ref (list->vector (string->list source)) position)]
                  [rest (string-take-right source (- (string-length source) position 1))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            ["invalid character name #\\~a" 
              (let* ([position (- (caddr (condition-irritants e)) 2)]
                  [head (if (zero? position) "" (string-take source position))]
                  [what (caadr (condition-irritants e))]
                  [l (+ 2 (string-length what))]
                  [rest (string-take-right source (- (string-length source) position l))]
                  [blank (make-string l #\space)])
                (private:tolerant-parse->patch (string-append head blank rest)))]
            ["invalid delimiter ~a for ~a"
              (let* ([position (caddr (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [end-position (string-find-delimiter source (+ 1 position))]
                  [l (- end-position position -1)]
                  [rest (string-take-right source (- (string-length source) position l))]
                  [blank (make-string l #\space)])
                (private:tolerant-parse->patch (string-append head blank rest)))]
            ["invalid character ~c in string hex escape"
              (let* ([position (caddr (condition-irritants e))]
                  [head (string-take source position)]
                  [rest (string-take-right source (- (string-length source) position 1))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [else 
              (display-condition e)
              (newline)
              (pretty-print (condition-irritants e))
              (pretty-print (car (condition-irritants e)))
            (raise 'can-not-tolerant0)]))]
        [(and (condition? e) (string? (caar (condition-irritants e))))
          (case (caar (condition-irritants e))
            [("unexpected dot (.)" "invalid sharp-sign prefix #~c" ) 
              (let* ([position (caddar (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [what (vector-ref (list->vector (string->list source)) position)]
                  [rest (string-take-right source (- (string-length source) position 1))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("unexpected close parenthesis" "unexpected close bracket" "unexpected end-of-file reading ~a")
              (let* ([position (caddar (condition-irritants e))]
                  [head (if (zero? position) "" (string-take source position))]
                  [what (vector-ref (list->vector (string->list source)) position)]
                  [rest (string-take-right source (- (string-length source) position 1))])
                (private:tolerant-parse->patch (string-append head " " rest)))]
            [("parenthesized list terminated by bracket" "bracketed list terminated by parenthesis")
              (let* ([position (- (caddar (condition-irritants e)) 1)]
                  [head (if (zero? position) "" (string-take source position))]
                  [what (vector-ref (list->vector (string->list source)) position)]
                  [rest (string-take-right source (- (string-length source) position 1))])
                (private:tolerant-parse->patch (string-append head ")" rest)))]
            [else (raise 'can-not-tolerant1)])]
        [else (raise 'can-not-tolerant2)]))))

(define source-file->annotations
  (case-lambda
    ([path] (source-file->annotations (read-string path) path))
    ([source path] (source-file->annotations source path (consume-sps-auxiliary source) #t))
    ([source path start-position tolerant?]
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
                  (let ([after (private:tolerant-parse->patch source)])
                    (if (= (string-length after) (string-length source))
                      (source-file->annotations after path start-position #f)
                      (raise 'can-not-tolerant)))]
                [(condition? e) 
                  (pretty-print `(format ,(condition-message e) ,@(condition-irritants e)))
                  (pretty-print path)]
                [else 
                  (pretty-print e)
                  (pretty-print path)
                  '()]))))))))
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
    [else (string-find-delimiter s (+ 1 position))]))
)