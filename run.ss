(import
    (chezscheme)
    (srfi :37 args-fold)
    (scheme-langserver))

(define (display-help)
  (let ([prog-name (car (command-line))])
    (format (current-error-port) "Usage:
  ~a --help | -h
  ~a [input-port] [output-port] [log-path]

Arguments:
  input-port                Port to read messages (default: stdin)
  output-port               Port to write messages (default: stdout)
  log-path                  Path to write log output (default: null)

Example Usage:
  ~a /path/to/scheme-langserver.log\n"
            prog-name prog-name prog-name)))

(define options
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg seeds)
             (display-help)
             (exit 0)))
   ;; (option '("multi-thread") #f #f
   ;;         (lambda (opt name arg seeds)
   ;;           (scheme-lsp-args-multi-thread-set! seeds #t)
   ;;           seeds))
   ;; (option '("type-inference") #f #f
   ;;         (lambda (opt name arg seeds)
   ;;           (scheme-lsp-args-type-inference-set! seeds #t)
   ;;           seeds))
   ))

(let* ([args (args-fold
              (command-line-arguments)
              options
              (lambda (opt name arg seeds)
                (format (current-error-port) "Unrecognized option: ~a\n" name)
                (display-help)
                (exit 0))
              (lambda (operand seeds)
                (cons operand seeds))
              '())]
       [operands (reverse args)])
  ;; TODO: use options
  (apply init-server operands))
