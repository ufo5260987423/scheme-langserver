(import
    (chezscheme)
    (srfi :37 args-fold)
    (scheme-langserver))

(define (display-help)
  (let ([prog-name (car (command-line))])
    (format (current-error-port) "Usage:
  
  ~a [option] ...

Options:
  -l, --log-path                Path to write log output. (default: current-project-directory/.scheme-langserver.log)
  -m, --multi-thread            Enable multi thread. (default: enable)

  -t, --type-inference          Enable type inference. (default: enable)

  -c, --cache-path              Directory to read/write workspace FASL cache. (default: disabled)

  -p, --package-manager         Package manager preset: akku (default) or txt.
  -f, --file-filter             File extension filter. Can be given multiple times,
                                e.g. -f scm.txt -f scm -f sls.
                                Extensions may be given with or without a leading dot.

  -h, --help                    Print help information.

  -v, --version                 Print version information.

  -e, --top-environment         Switch between different top environments, for example R6RS, R7RS, s7, goldfish, etc.(default: R6RS)


Example Usage:
  ~a -l /path/to/scheme-langserver.log\n"
      prog-name prog-name)))

(define (private:get-version)
  (or
    (guard (e [else #f])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports "git describe --tags --always --dirty 2>/dev/null"
                                        (buffer-mode block)
                                        (native-transcoder))])
        (let ([line (get-line from-stdout)])
          (close-output-port to-stdin)
          (close-input-port from-stdout)
          (close-input-port from-stderr)
          (if (or (eof-object? line) (string=? line ""))
            #f
            line))))
    (guard (e [else #f])
      (call-with-input-file ".version"
        (lambda (p)
          (let ([line (get-line p)])
            (if (eof-object? line) #f line)))))
    "2.1.8"))

(define version (private:get-version))

(define default-log-path "./.scheme-langserver.log")
(define default-multi-thread #t)
(define default-type-inference #t)
(define default-top-environment 'r6rs)
(define default-cache-path #f)
(define default-package-manager 'akku)

;; Normalizes accumulated file-filter extension strings into a list.
;; The result is sorted so that equivalent option orderings produce the
;; same cache manifest.
(define (private:normalize-file-filter accumulated)
  (let ([deduped
          (let ([seen (make-hashtable string-hash equal?)] [result '()])
            (for-each
              (lambda (item)
                (when (string? item)
                  (unless (hashtable-ref seen item #f)
                    (hashtable-set! seen item #t)
                    (set! result (cons item result)))))
              accumulated)
            (reverse result))])
    (if (null? deduped) '() (list-sort string<? deduped))))

;; Resolves the final file-filter configuration from package-manager and
;; file-filter options. They are mutually exclusive.
(define (private:resolve-file-filter package-manager extensions)
  (cond
    [(and package-manager (not (null? extensions)))
     (display "Error: --package-manager and --file-filter cannot be used together.\n")
     (display-help)
     (exit 1)]
    [(not (null? extensions)) extensions]
    [package-manager package-manager]
    [else 'akku]))

(define (private:ensure-leading-dot str)
  (if (and (> (string-length str) 0) (char=? (string-ref str 0) #\.))
    str
    (string-append "." str)))

(define (make-default-options)
  (let ((ht (make-hashtable string-hash equal?)))
    (hashtable-set! ht "log-path" default-log-path)
    (hashtable-set! ht "multi-thread" default-multi-thread)
    (hashtable-set! ht "type-inference" default-type-inference)
    (hashtable-set! ht "top-environment" default-top-environment)
    (hashtable-set! ht "cache-path" default-cache-path)
    (hashtable-set! ht "file-filter" '())
    (hashtable-set! ht "package-manager" #f)
    ht))

(define (log-path-proc option name arg seeds)
  (hashtable-set! seeds "log-path" arg)
  seeds)

(define (boolean-option-proc key)
  (lambda (option name arg seeds)
    (cond
      [(string-ci=? arg "enable")
        (hashtable-set! seeds key #t)]
      [(string-ci=? arg "disable")
        (hashtable-set! seeds key #f)])
    seeds))

(define multi-thread-proc (boolean-option-proc "multi-thread"))
(define type-inference-proc (boolean-option-proc "type-inference"))

(define (cache-path-proc option name arg seeds)
  (hashtable-set! seeds "cache-path" arg)
  seeds)

(define (package-manager-parse str)
  (cond
    [(string-ci=? str "akku") 'akku]
    [(string-ci=? str "txt") 'txt]
    [else #f]))

(define (package-manager-proc option name arg seeds)
  (let ([val (package-manager-parse arg)])
    (if val
      (begin
        (hashtable-set! seeds "package-manager" val)
        seeds)
      (begin
        (display "Invalid value for --package-manager. Valid values: akku, txt\n")
        (exit 1)))))

(define (private:split-string str char)
  (let loop ([i 0] [start 0] [result '()])
    (if (= i (string-length str))
      (reverse (cons (substring str start i) result))
      (if (char=? (string-ref str i) char)
        (loop (+ i 1) (+ i 1) (cons (substring str start i) result))
        (loop (+ i 1) start result)))))

(define (file-filter-parse str)
  (map private:ensure-leading-dot (private:split-string str #\,)))

(define (file-filter-proc option name arg seeds)
  (hashtable-set! seeds "file-filter"
    (append (hashtable-ref seeds "file-filter" '()) (file-filter-parse arg)))
  seeds)

(define (top-environment-parse str)
  (cond
    ((string-ci=? str "r6rs") 'r6rs)
    ((string-ci=? str "r7rs") 'r7rs)
    ((string-ci=? str "s7") 's7)
    ((string-ci=? str "goldfish") 's7)
    (else #f)))

(define (top-environment-proc option name arg seeds)
  (let ((val (top-environment-parse arg)))
    (if val
      (begin
        (hashtable-set! seeds "top-environment" val)
        seeds)
      (begin
        (display "Invalid value for --top-environment. Valid values: r6rs, r7rs, s7\n")
        (exit 1)))))

(define options
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg seeds)
             (display-help)
             (exit 0)))
   (option '(#\v "version") #f #f
           (lambda (opt name arg seeds)
             (format (current-output-port) "scheme-langserver ~a\n" version)
             (exit 0)))
    (option '(#\l "log-path") #t #f
           log-path-proc)
    (option '(#\m "multi-thread") #t #f
           multi-thread-proc)
    (option '(#\t "type-inference") #t #f
           type-inference-proc)
    (option '(#\e "top-environment") #t #f
           top-environment-proc)
    (option '(#\c "cache-path") #t #f
           cache-path-proc)
    (option '(#\p "package-manager") #t #f
           package-manager-proc)
    (option '(#\f "file-filter") #t #f
           file-filter-proc)))

(let* ([args 
        (args-fold
          (command-line-arguments)
          options
          (lambda (opt name arg seeds)
            (format (current-error-port) "Unrecognized option: ~a\n" name)
            (display-help)
            (exit 1))
          (lambda (operand seeds)
            seeds)
          (make-default-options))])
  (init-server
    (standard-input-port)
    (standard-output-port)
    (open-file-output-port 
      (hashtable-ref args "log-path" default-log-path) 
      (file-options replace) 
      'block 
      (make-transcoder (utf-8-codec)))
    (hashtable-ref args "multi-thread" default-multi-thread)
    (hashtable-ref args "type-inference" default-type-inference)
    (hashtable-ref args "top-environment" default-top-environment)
    #f
    (hashtable-ref args "cache-path" default-cache-path)
    (private:resolve-file-filter
      (hashtable-ref args "package-manager" #f)
      (private:normalize-file-filter (hashtable-ref args "file-filter" '())))))
