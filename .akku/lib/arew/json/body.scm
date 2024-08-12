(define json-nesting-depth-limit (make-parameter 99))

(define (json-null? obj)
  (eq? obj 'null))

(define-record-type <json-error>
  (make-json-error reason)
  json-error?
  (reason json-error-reason))

(define (json-whitespace? char)
  (case char
    ((#\x20 ; Space
      #\x09 ; Horizontal tab
      #\x0A ; Line feed or New line
      #\x0D
      #\x1E  ;; Record Separator
      )
     #t)
    (else #f)))

(define (expect value other)
  (when (eof-object? value)
    (raise (make-json-error "Unexpected end-of-file.")))
  ;; (unless (char=? value other)
  ;;   (raise (make-json-error "Unexpected character."))))
  (void))

(define (port->generator port)
  (lambda ()
    (read-char port)))

(define (%json-tokens generator)

  (define (maybe-ignore-whitespace generator)
    (let loop ((char (generator)))
      (if (json-whitespace? char)
          (loop (generator))
          char)))

  (define (expect-null generator)
    (expect (generator) #\u)
    (expect (generator) #\l)
    (expect (generator) #\l))

  (define (expect-true generator)
    (expect (generator) #\r)
    (expect (generator) #\u)
    (expect (generator) #\e))

  (define (expect-false generator)
    (expect (generator) #\a)
    (expect (generator) #\l)
    (expect (generator) #\s)
    (expect (generator) #\e))

  (define (maybe-char generator)
    (let ((char (generator)))
      (when (eof-object? char)
        (raise (make-json-error "Unexpected end-of-file.")))
      (when (char=? char #\")
        (raise (make-json-error "Unexpected end of string.")))
      char))

  (define (read-unicode-escape generator)
    (let* ((one (maybe-char generator))
           (two (maybe-char generator))
           (three (maybe-char generator))
           (four (maybe-char generator)))
      (let ((out (string->number (list->string (list one two three four)) 16)))
        (if out
            out
            (raise (make-json-error "Invalid code point."))))))

  (define (read-json-string generator)
    (let loop ((char (generator))
               (out '()))
      (when (eof-object? char)
        (raise (make-json-error "Unexpected end of file.")))

      ;; (when (or (char=? char #\x00)
      ;;           (char=? char #\newline)
      ;;           (char=? char #\tab))
      ;;   (raise (make-json-error "Unescaped control char.")))

      ;; XXX: Here be dragons.
      (cond
       ((char=? char #\\)
        (begin
          (let loop-unescape ((char (generator))
                              (chars-unescaped '()))
            (case char
              ((#\" #\\ #\/) (loop (generator)
                                    (cons char (append chars-unescaped
                                                      out))))
              ((#\b) (loop (generator) (cons #\backspace
                                           (append chars-unescaped
                                                   out))))
              ;; racket does not support chars as hex
              ;; ((#\f) (loop (generator) (cons #\x0c
              ;;                              (append chars-unescaped
              ;;                                      out))))
              ((#\n) (loop (generator) (cons #\newline
                                           (append chars-unescaped
                                                   out))))
              ;; racket does not support chars as hex
              ;; ((#\r) (loop (generator) (cons #\x0D
              ;;                              (append chars-unescaped
              ;;                                      out))))
              ((#\t) (loop (generator) (cons #\tab
                                           (append chars-unescaped
                                                   out))))
              ;; TODO: remove this code that try to parse escaped
              ;; unicodes.
              ((#\u) (let loop-unicode ((code1 (read-unicode-escape generator))
                                      (chars chars-unescaped))
                     (let ((next-char (generator)))
                       (if (and (<= #xd800 code1 #xdbff)
                                (char=? next-char #\\))
                           (if (char=? (generator) #\u)
                               (let ((code2 (read-unicode-escape generator)))
                                 (if (<= #xdc00 code2 #xdfff)
                                     (let ((integer
                                            (+ #x10000 (bitwise-ior
                                                        (ash (- code1 #xd800) 10)
                                                        (- code2 #xdc00)))))
                                       ;; full escape of unicode is parsed...
                                       (loop (generator)
                                             (cons (integer->char integer)
                                                   (append chars
                                                           out))))
                                     ;; This is another unicode char
                                     (loop-unicode (read-unicode-escape generator)
                                                   (cons (integer->char code1) chars))))
                               ;; The escaped unicode char is
                               ;; parsed, need to parse another
                               ;; escape that is not a unicode
                               ;; escape sequence
                               (loop-unescape char (cons (integer->char code1)
                                                         chars)))
                           ;; This is not a big-ish unicode char and
                           ;; the next thing is some other char.
                           (loop next-char
                                 (cons (integer->char code1) (append chars out)))))))
              (else (raise (make-json-error "Unexpected escaped sequence.")))))))
       ((char=? char #\")
        (list->string (reverse out)))
       (else
        (loop (generator) (cons char out))))))

  (define (maybe-read-number char generator)
    ;; accumulate chars until a control char or whitespace is reached,
    ;; validate that it is JSON number, then intrepret it as Scheme
    ;; number using string->number
    (let loop ((char char)
               (out '()))
      (if (or (eof-object? char)
              (json-whitespace? char)
              (char=? char #\,)
              (char=? char #\])
              (char=? char #\}))
          (let ((string (list->string (reverse out))))
            (let ((number (string->number string)))
              (if number
                  (values number char)
                  (raise (make-json-error (format #f "Invalid number: ~s" string))))))
          (loop (generator) (cons char out)))))

  (define char (maybe-ignore-whitespace generator))

  ;; gist
  (lambda ()
    (if (eof-object? char)
        char ;; return that eof-object
        (case char
          ((#\n) (expect-null generator) (set! char (maybe-ignore-whitespace generator)) 'null)
          ((#\t) (expect-true generator) (set! char (maybe-ignore-whitespace generator)) #t)
          ((#\f) (expect-false generator) (set! char (maybe-ignore-whitespace generator)) #f)
          ((#\:) (set! char (maybe-ignore-whitespace generator)) 'colon)
          ((#\,) (set! char (maybe-ignore-whitespace generator)) 'comma)
          ((#\[) (set! char (maybe-ignore-whitespace generator)) 'array-start)
          ((#\]) (set! char (maybe-ignore-whitespace generator)) 'array-end)
          ((#\{) (set! char (maybe-ignore-whitespace generator)) 'object-start)
          ((#\}) (set! char (maybe-ignore-whitespace generator)) 'object-end)
          ((#\") (let ((out (read-json-string generator)))
                   (set! char (maybe-ignore-whitespace generator))
                   out))
          (else
           (call-with-values (lambda () (maybe-read-number char generator))
             (lambda (number next)
               (if (json-whitespace? next)
                   (set! char (maybe-ignore-whitespace generator))
                   (set! char next))
               number)))))))

(define json-tokens
  (lambda args
    (if (null? args)
        (json-tokens (current-input-port))
        (let ((port-or-generator (car args)))
          (cond
           ((procedure? port-or-generator)
            (%json-tokens port-or-generator))
           ;; racket does not like the original test
           (#;(and (textual-port? port-or-generator) (input-port? port-or-generator))
            (port? port-or-generator)
            (%json-tokens (port->generator port-or-generator)))
           (else (error 'json "json-tokens error, argument is not valid" port-or-generator)))))))

(define (list->reverse-vector objs length)
  (define vector (make-vector length))
  (let loop ((objs objs)
             (index (fx- length 1)))
    (if (null? objs)
        vector
        (begin
          (vector-set! vector index (car objs))
          (loop (cdr objs) (fx- index 1))))))

(define json-read
  (lambda args
    (if (null? args)
        (json-read (current-input-port))
        (let ((nesting-depth-remaining (json-nesting-depth-limit)))

          (define nesting-depth-remaining-increment!
            (lambda ()
              (set! nesting-depth-remaining (fx+ nesting-depth-remaining 1))))

          (define nesting-depth-remaining-decrement!
            (lambda ()
              (if (fxzero? nesting-depth-remaining)
                  (raise (make-json-error "Maximum recursion depth exceeded."))
                  (set! nesting-depth-remaining (fx- nesting-depth-remaining 1)))))

          (define (read token generator)
            (cond
             ((or (number? token) (string? token) (boolean? token) (json-null? token))
              token)
             ((eq? token 'array-start)
              (let ((next (generator)))
                (if (eq? next 'array-end)
                    (begin
                      (nesting-depth-remaining-increment!)
                      (make-vector 0))
                    (let loop ((out (list (read next generator)))
                               (length 1))
                      (case (generator)
                        ((comma) (loop (cons (read (generator) generator) out)
                                       (fx+ length 1)))
                        ((array-end)
                         (nesting-depth-remaining-increment!)
                         (list->reverse-vector out length))
                        (else (raise (make-json-error "Invalid array."))))))))
             ((eq? token 'object-start)
              (nesting-depth-remaining-decrement!)
              (let loop ((out '()))
                (let ((next (generator)))
                  (if (eq? next 'object-end)
                      (begin (nesting-depth-remaining-increment!) out)
                      (let* ((key (string->symbol next))
                             (colon (generator))
                             (value (read (generator) generator)))
                        (case (generator)
                          ((comma) (loop (cons (cons key value) out)))
                          ((object-end)
                           (nesting-depth-remaining-increment!)
                           (cons (cons key value) out))
                          (else (raise (make-json-error "Invalid object.")))))))))))

          (let* ((generator (json-tokens (car args)))
                 (token (generator)))
            (guard (ex (else (raise (make-json-error "Invalid JSON"))))
                   (read token generator)))))))

;; write procedures

(define (json-accumulator accumulator)

  (define (write-json-char char accumulator)
    (case char
      ((#\x00) (accumulator "\\u0000"))
      ((#\") (accumulator "\\\""))
      ((#\\) (accumulator "\\\\"))
      ((#\/) (accumulator "\\/"))
      ((#\return) (accumulator "\\r"))
      ((#\newline) (accumulator "\\n"))
      ((#\tab) (accumulator "\\t"))
      ((#\backspace) (accumulator "\\b"))
      ((#\x0c) (accumulator "\\f"))
      (else (accumulator char))))

  (define (write-json-string string accumulator)
    (accumulator #\")
    (string-for-each
     (lambda (char) (write-json-char char accumulator))
     string)
    (accumulator #\"))

  (define (write-json-value obj accumulator)
    (cond
     ((eq? obj 'null) (accumulator "null"))
     ((boolean? obj) (if obj
                         (accumulator "true")
                         (accumulator "false")))
     ((string? obj) (write-json-string obj accumulator))
     ((number? obj) (accumulator (number->string obj)))
     (else (raise (make-json-error "Invalid json value.")))))

  (define (raise-invalid-event event)
    (raise (make-json-error "json-accumulator: invalid event.")))

  (define (object-start k)
    (lambda (accumulator event)
      (accumulator #\{)
      (case (car event)
        ((json-value)
         (let ((key (cdr event)))
           (unless (symbol? key) (raise-invalid-event event))
           (write-json-string (symbol->string key) accumulator)
           (object-value k)))
        ((json-structure)
         (case (cdr event)
           ((object-end)
            (accumulator #\})
            k)
           (else (raise-invalid-event event))))
        (else (raise-invalid-event event)))))

  (define (object-value k)
    (lambda (accumulator event)
      (accumulator #\:)
      (case (car event)
        ((json-value)
         (write-json-value (cdr event) accumulator)
         (object-maybe-continue k))
        ((json-structure)
         (case (cdr event)
           ((array-start)
            (array-start (object-maybe-continue k)))
           ((object-start)
            (object-start (object-maybe-continue k)))
           (else (raise-invalid-event event))))
        (else (raise-invalid-event event)))))

  (define (object-maybe-continue k)
    (lambda (accumulator event)
      (case (car event)
        ((json-value)
         (accumulator #\,)
         (let ((key (cdr event)))
           (unless (symbol? key) (raise-invalid-event event))
           (write-json-value (symbol->string key) accumulator)
           (object-value k)))
        ((json-structure)
         (case (cdr event)
           ((object-end)
            (accumulator #\})
            k)
           (else (raise-invalid-event event))))
        (else (raise-invalid-event event)))))

  (define (array-start k)
    (lambda (accumulator event)
      (accumulator #\[)
      (case (car event)
        ((json-value)
         (write-json-value (cdr event) accumulator)
         (array-maybe-continue k))
        ((json-structure)
         (case (cdr event)
           ((array-end)
            (accumulator #\])
            k)
           ((array-start) (array-start (array-maybe-continue k)))
           ((object-start) (object-start (array-maybe-continue k)))
           (else (raise-invalid-event event))))
        (else (raise-invalid-event event)))))

  (define (array-maybe-continue k)
    (lambda (accumulator event)
      (case (car event)
        ((json-value)
         (accumulator #\,)
         (write-json-value (cdr event) accumulator)
         (array-maybe-continue k))
        ((json-structure)
         (case (cdr event)
           ((array-end)
            (accumulator #\])
            k)
           ((array-start)
            (accumulator #\,)
            (array-start (array-maybe-continue k)))
           ((object-start)
            (accumulator #\,)
            (object-start (array-maybe-continue k)))
           (else (raise-invalid-event event))))
        (else (raise-invalid-event event)))))

  (define (start accumulator event)
    (case (car event)
      ((json-value)
       (write-json-value (cdr event) accumulator)
       raise-invalid-event)
      ((json-structure)
       (case (cdr event)
         ((array-start)
          (array-start raise-invalid-event))
         ((object-start)
          (object-start raise-invalid-event))
         (else (raise-invalid-event event))))
      (else (raise-invalid-event event))))

  (let ((k start))
    (lambda (event)
      (set! k (k accumulator event)))))

(define (%json-write obj accumulator)

  (define (raise-unless-valid? obj)

    (cond
     ((null? obj) (void))
     ((eq? obj 'null) (void))
     ((boolean? obj) (void))
     ((string? obj) (void))
     ((and (number? obj)
           (not (infinite? obj))
           (not (nan? obj))
           (real? obj)
           (or (and (exact? obj) (= (denominator obj) 1))
               (inexact? obj)))
      (void))
     ((vector? obj)
      (vector-for-each (lambda (obj) (raise-unless-valid? obj)) obj))
     ;; XXX: use pair? then recursively check the tail.
     ((pair? obj)
      (for-each (lambda (obj)
                  (unless (pair? obj)
                    (raise (make-json-error "Unexpected object, not a pair.")))
                  (unless (symbol? (car obj))
                    (raise (make-json-error "Unexpected object, not a symbol key.")))
                  (raise-unless-valid? (cdr obj)))
                obj))
     (else (raise (make-json-error "Unexpected object")))))

  (define (write obj accumulator)
    (cond
     ((or (eq? obj 'null)
          (boolean? obj)
          (string? obj)
          (symbol? obj)
          (number? obj))
      (accumulator (cons 'json-value obj)))
     ((vector? obj)
      (accumulator '(json-structure . array-start))
      (vector-for-each (lambda (obj) (write obj accumulator)) obj)
      (accumulator '(json-structure . array-end)))
     ((null? obj)
      (accumulator '(json-structure . object-start))
      (accumulator '(json-structure . object-end)))
     ((pair? obj)
      (accumulator '(json-structure . object-start))
      (for-each (lambda (pair)
                  (write (car pair) accumulator)
                  (write (cdr pair) accumulator))
                obj)
      (accumulator '(json-structure . object-end)))
     (else (error 'ruse-json "Unexpected error!"))))

  (raise-unless-valid? obj)
  (write obj (json-accumulator accumulator)))

(define (port->accumulator port)
  (lambda (char-or-string)
    (cond
     ((char? char-or-string) (put-char port char-or-string))
     ((string? char-or-string) (put-string port char-or-string))
     (else (raise (make-json-error "Not a char or string"))))))

(define json-write
  (lambda (obj . args)
    (if (null? args)
        (json-write obj (current-input-port))
        (if (procedure? (car args))
            (%json-write obj (car args))
            (%json-write obj (port->accumulator (car args)))))))
