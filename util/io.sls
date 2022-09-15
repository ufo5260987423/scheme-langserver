(library (scheme-langserver util io)
    (export 
        read-lines 
        read-line
        write-lines

        read-string
        write-string
        )
    (import (rnrs) )

(define (write-lines lines path)
    (if (not (null? lines))
        (call-with-output-file path
            (lambda(port)
                (let loop ((lines lines))
                    (write-string (car lines) port)
                    (if (not (null? (cdr lines)))
                        (begin 
                            (write-string "\n" port)
                            (loop (cdr lines)))))))))

(define (write-string s port)
    (let loop ((l (string->list s)))
        (if (null? l)
            '()
            (begin 
                (write-char (car l) port)
                (loop (cdr l))))))

(define (read-lines path)
    (call-with-input-file path
        (lambda (port)
            (let loop ((result '()) (item (read-line port)))
                (if (eof-object? item)
                result
                (loop (append result (list item)) (read-line port)))))))

(define (read-line . port)
    (let ((eat (lambda (p c)
            (if (and (not (eof-object? (peek-char p))) (char=? (peek-char p) c))
                (read-char p)))))
        (let ((p (if (null? port) (current-input-port) (car port))))
            (let loop ((c (read-char p)) (line '()))
                (cond 
                    ((eof-object? c) (if (null? line) c (list->string (reverse line))))
                    ((char=? #\newline c) (eat p #\return) (list->string (reverse line)))
                    ((char=? #\return c) (eat p #\newline) (list->string (reverse line)))
                    (else (loop (read-char p) (cons c line))))))))

(define (read-string path)
    (call-with-input-file path
        (lambda (port)
            (let loop ((c (read-char port)) (line '()))
                (cond 
                    ((eof-object? c) (if (null? line) c (list->string (reverse line))))
                    (else (loop (read-char port) (cons c line))))))))
)