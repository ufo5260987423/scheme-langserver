(import
    (chezscheme)
    (srfi :37 args-fold)
    (scheme-langserver))

(define (display-help)
  (let ([prog-name (car (command-line))])
    (display (format "Usage:
  ~a --help | -h
  ~a [options] [input-port] [output-port] [log-path]

Options:
  --multi-thread            enable multi-thread
  --type-inference          enable type inference

Arguments:
  input-port                Port to read messages (default: stdin)
  output-port               Port to write messages (default: stdout)
  log-path                  Path to write log output (default: null)

Example Usage:
  ~a /path/to/scheme-langserver.log\n"
                     prog-name prog-name prog-name))))

(define-record-type scheme-lsp-args
  (fields
   (mutable multi-thread)
   (mutable type-inference)
   ;; Reversed oprands
   (mutable oprands)))

(define options
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg seeds)
             (display-help)
             (exit 0)))
   (option '("multi-thread") #f #f
           (lambda (opt name arg seeds)
             (scheme-lsp-args-multi-thread-set! seeds #t)
             seeds))
   (option '("type-inference") #f #f
           (lambda (opt name arg seeds)
             (scheme-lsp-args-type-inference-set! seeds #t)
             seeds))))

(let* ([args (args-fold
              (command-line-arguments)
              options
              (lambda (opt name arg seeds)
                (format #t "Unrecognized option: ~a\n" name)
                (display-help)
                (exit 0))
              (lambda (operand seeds)
                (scheme-lsp-args-oprands-set!
                 seeds
                 (cons operand
                       (scheme-lsp-args-oprands seeds)))
                seeds)
              (make-scheme-lsp-args #f #f '()))]
       [operands (reverse (scheme-lsp-args-oprands args))])
  ;; TODO: use options
  (apply init-server operands))
