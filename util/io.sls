(library (scheme-langserver util io)
  (export
    read-lines
    read-to-CRNL
    read-string)
  (import (rnrs))

(define (read-to-CRNL port)
  (let loop ([tail '()]
    [current-char (get-u8 port)])
      (cond
        [(eof-object? current-char)
          (utf8->string (u8-list->bytevector (reverse tail)))]
        [(and
          (= (char->integer #\return ) current-char)
          (= (char->integer #\newline) (lookahead-u8 port)))
          (get-u8 port) ;; Consume \n
          (utf8->string (u8-list->bytevector (reverse tail)))]
        [(= (char->integer #\newline) current-char)
          (utf8->string (u8-list->bytevector (reverse tail)))]
        [else (loop (cons current-char tail) (get-u8 port))])))

(define (read-lines path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((result '()) (item (get-line port)))
        (if (eof-object? item)
          (reverse result)
          (loop (cons item result) (get-line port)))))))

(define (read-string path)
  (call-with-input-file path
    (lambda (port)
      (get-string-all port))))
)
