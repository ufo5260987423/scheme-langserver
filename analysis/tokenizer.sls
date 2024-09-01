(library (scheme-langserver analysis tokenizer)
  (export 
    source-file->annotations
    fault-tolerant->init-index-node)
  (import 
    (chezscheme) 
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver util io)
    (scheme-langserver util try))

(define (fault-tolerant->init-index-node path)
  (let loop ([source (read-string path)] [bias '()])
    (try 
      (map 
        (lambda (item) (init-index-node '() item)) 
        (source-file->annotations source path))
      (except e
        [(condition? e)
          (if 
            (or
              (equal? (condition-message e) "bracketed list terminated by parenthesis") 
              (equal? (condition-message e) "parenthesized list terminated by bracket") 
              (equal? (condition-message e) "unexpected close parenthesis") 
              (equal? (condition-message e) "unexpected end-of-file reading ~a"))
            (apply loop (private-parse&attach source)))]
        [else 
          (pretty-print path)]))))

(define (private-parse&attach source)
)

(define source-file->annotations
  (case-lambda
    ([path] (source-file->annotations (read-string path) path))
    ([source path] (source-file->annotations source path (consume-sps-auxiliary source)))
    ([source path start-position]
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
                [else 
                  (pretty-print `(format ,(condition-message e) ,@(condition-irritants e)))
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
        [(eqv? c #\;) (loop (get-char ip) #t)]
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
)