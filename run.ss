(import
    (chezscheme)
    (srfi :37 args-fold)
    (scheme-langserver))

(define (display-help)
  (let ([prog-name (car (command-line))])
    (format (current-error-port) "Usage:
  
  ~a [option] ...

Options:
  -l, --log-path                Path to write log output (default: current-project-directory/.scheme-langserver.log)
  -m, --multi-thread            Enable multi thread (default: enable).

  -t, --type-inference          Enable type inference (default: enable).


  -h, --help                    Print help information

  -e, --top-environment         Switch to support different top environment, for example R6RS, R7RS, etc.(default: R6RS)


Example Usage:
  ~a -l /path/to/scheme-langserver.log\n"
      prog-name prog-name)))

(define default-log-path "./.scheme-langserver.log")
(define default-multi-thread #t)
(define default-type-inference #t)
(define default-top-environment "R6RS")

(define (make-default-options)
  (let ((ht (make-hashtable string-hash equal?)))
    (hashtable-set! ht "log-path" default-log-path)
    (hashtable-set! ht "multi-thread" default-multi-thread)
    (hashtable-set! ht "type-inference" default-type-inference)
    (hashtable-set! ht "top-environment" default-top-environment)
    ht))

(define (log-path-proc option name arg seeds)
  (hashtable-set! seeds "log-path" arg)
  seeds)

(define (multi-thread-proc option name arg seeds)
  (cond
    ((string-ci=? arg "enable")
      (hashtable-set! seeds "multi-thread" #t))
    ((string-ci=? arg "disable")
      (hashtable-set! seeds "multi-thread" #f)))
  seeds)

(define (type-inference-proc option name arg seeds)
  (cond
    ((string-ci=? arg "enable")
      (hashtable-set! seeds "type-inference" #t))
    ((string-ci=? arg "disable")
      (hashtable-set! seeds "type-inference" #f)))
  seeds)

(define (top-environment-parse str)
  (cond
    ((string-ci=? str "r6rs") 'r6rs)
    ((string-ci=? str "r7rs") 'r7rs)
    ((string-ci=? str "s7") 's7)
    ((string-ci=? str "goldfish") 'goldfish)
    (else #f)))


(define (top-environment-proc option name arg seeds)
(let ((val (top-environment-parse arg)))
  (if val
    (begin
      (hashtable-set! seeds "top-environment" val)
      seeds)
    (begin
      (display "Invalid value for --top-environment. Valid values: r6rs, r7rs, s7, goldfish\n")
      (exit 1)))))

(define options
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg seeds)
             (display-help)
             (exit 0)))
    (option '(#\l "log-path") #t #f
           log-path-proc)
    (option '(#\m "multi-thread") #t #f
           multi-thread-proc)
    (option '(#\t "type-inference") #t #f
           type-inference-proc)
    (option '(#\e "top-environment") #t #f
           top-environment-proc)
))

(let* ([args (args-fold
              (command-line-arguments)
              options
              (lambda (opt name arg seeds)
                (format (current-error-port) "Unrecognized option: ~a\n" name)
                (display-help)
                (exit 0))
              (lambda (operand seeds)
                seeds)
              (make-default-options))])
  ;; TODO: use options
  ;; (apply init-server operands)
  (init-server
    (standard-input-port)
    (standard-output-port)
    (open-file-output-port 
      (hashtable-ref args "log-path" default-log-path) 
      (file-options replace) 
      'block 
      (make-transcoder (utf-8-codec))) ;; log port
    (hashtable-ref args "multi-thread" default-multi-thread)
    (hashtable-ref args "type-inference" default-type-inference)
    (hashtable-ref args "top-environment" default-top-environment)))
