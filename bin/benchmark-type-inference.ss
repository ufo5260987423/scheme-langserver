#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Performance benchmark for type inference subsystem.
;; Run: source .akku/bin/activate && scheme --script benchmark-type-inference.ss
#!r6rs

(import
  (chezscheme)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier meta)
  (scheme-langserver analysis type substitutions generator)
  (scheme-langserver analysis type domain-specific-language interpreter)
  (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (scheme-langserver analysis type substitutions util)
  (scheme-langserver protocol apis hover)
  (scheme-langserver util path)
  (scheme-langserver util association)
  (scheme-langserver util test))

;; ---------------------------------------------------------------------------
;; Benchmark harness
;; ---------------------------------------------------------------------------

(define-record-type benchmark-result
  (fields name real-ms cpu-ms gc-ms result-count))

(define (current-real-ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t))
       (div (time-nanosecond t) 1000000))))

;; cpu-time returns a fixnum (ms) in script mode, not a time record.
;; sstats fields may also return numbers depending on the context.
(define (maybe-time->ms x)
  (if (record? x)
      (+ (* 1000 (time-second x)) (div (time-nanosecond x) 1000000))
      x))

(define (run-once name thunk)
  (collect)
  (let ([real0 (current-real-ms)]
        [cpu0  (cpu-time)]
        [gc0   (maybe-time->ms (sstats-gc-cpu (statistics)))])
    (let ([result (thunk)])
      (let ([real1 (current-real-ms)]
            [cpu1  (cpu-time)]
            [gc1   (maybe-time->ms (sstats-gc-cpu (statistics)))])
        (make-benchmark-result
          name
          (- real1 real0)
          (- cpu1 cpu0)
          (- gc1 gc0)
          (if (list? result) (length result) 0))))))

(define (run-benchmark name runs thunk)
  (let ([samples (let loop ([i 0] [acc '()])
                   (if (= i runs)
                       (reverse acc)
                       (loop (+ i 1) (cons (run-once name thunk) acc))))])
    (let ([avg-real (round (/ (apply + (map benchmark-result-real-ms samples)) runs))]
          [avg-cpu  (round (/ (apply + (map benchmark-result-cpu-ms samples)) runs))]
          [avg-gc   (round (/ (apply + (map benchmark-result-gc-ms samples)) runs))]
          [counts   (map benchmark-result-result-count samples)])
      (make-benchmark-result name avg-real avg-cpu avg-gc (car counts)))))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (setup-workspace path-root type-inference?)
  (init-workspace path-root 'akku 'r6rs #f type-inference?))

(define (find-def document name)
  (let ([root (car (document-index-node-list document))])
    (find-define-by-name root name)))

;; ---------------------------------------------------------------------------
;; Benchmark definitions
;; ---------------------------------------------------------------------------

(define benchmarks '())

(define (register-benchmark name runs thunk)
  (set! benchmarks (append benchmarks (list (cons name (cons runs thunk))))))

;; 1. init-workspace without type inference
(register-benchmark
  "init-workspace (no ti)" 3
  (lambda ()
    (setup-workspace (current-directory) #f)
    '()))

;; 2. init-workspace with type inference
(register-benchmark
  "init-workspace (with ti)" 1
  (lambda ()
    (setup-workspace (current-directory) #t)
    '()))

;; 3. construct-substitutions for binary-search document
(let ([workspace #f] [document #f])
  (register-benchmark
    "construct-substitutions binary-search" 3
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f))
        (let ([node (walk-file (workspace-file-node workspace)
                               (string-append (current-directory) "/util/binary-search.sls"))])
          (set! document (file-node-document node))))
      (construct-substitutions-for document)
      '())))

;; 4a. type:interpret-result-list for natural-order-compare (define node)
(let ([workspace #f] [document #f] [target-node #f])
  (register-benchmark
    "type:interpret natural-order-compare def" 3
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f))
        (let ([node (walk-file (workspace-file-node workspace)
                               (string-append (current-directory) "/util/natural-order-compare.sls"))])
          (set! document (file-node-document node))
          (construct-substitutions-for document)
          (set! target-node
            (find-define-by-name (car (document-index-node-list document)) 'natural-order-compare))))
      (type:interpret-result-list target-node))))

;; 4b. type:interpret-result-list for natural-order-compare (case-lambda node)
(let ([workspace #f] [document #f] [target-node #f])
  (register-benchmark
    "type:interpret natural-order-compare cl" 3
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f))
        (let ([node (walk-file (workspace-file-node workspace)
                               (string-append (current-directory) "/util/natural-order-compare.sls"))])
          (set! document (file-node-document node))
          (construct-substitutions-for document)
          (let ([def-node (find-define-by-name (car (document-index-node-list document)) 'natural-order-compare)])
            (set! target-node (cadr (index-node-children def-node))))))
      (type:interpret-result-list target-node))))

;; 5a. type:interpret-result-list for assq-ref (define node)
(let ([workspace #f] [document #f] [target-node #f])
  (register-benchmark
    "type:interpret assq-ref def" 3
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f))
        (let ([node (walk-file (workspace-file-node workspace)
                               (string-append (current-directory) "/util/association.sls"))])
          (set! document (file-node-document node))
          (construct-substitutions-for document)
          (set! target-node
            (find-define-by-name (car (document-index-node-list document)) 'assq-ref))))
      (type:interpret-result-list target-node))))

;; 5b. type:interpret-result-list for assq-ref (case-lambda node)
(let ([workspace #f] [document #f] [target-node #f])
  (register-benchmark
    "type:interpret assq-ref cl" 3
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f))
        (let ([node (walk-file (workspace-file-node workspace)
                               (string-append (current-directory) "/util/association.sls"))])
          (set! document (file-node-document node))
          (construct-substitutions-for document)
          (let ([def-node (find-define-by-name (car (document-index-node-list document)) 'assq-ref)])
            (set! target-node (cadr (index-node-children def-node))))))
      (type:interpret-result-list target-node))))

;; 6a. type:interpret-result-list for binary-search (define node)
(let ([workspace #f] [document #f] [target-node #f])
  (register-benchmark
    "type:interpret binary-search def" 1
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f))
        (let ([node (walk-file (workspace-file-node workspace)
                               (string-append (current-directory) "/util/binary-search.sls"))])
          (set! document (file-node-document node))
          (construct-substitutions-for document)
          (set! target-node
            (find-define-by-name (car (document-index-node-list document)) 'binary-search))))
      (type:interpret-result-list target-node))))

;; 6b. type:interpret-result-list for binary-search (case-lambda node)
(let ([workspace #f] [document #f] [target-node #f])
  (register-benchmark
    "type:interpret binary-search cl" 1
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f))
        (let ([node (walk-file (workspace-file-node workspace)
                               (string-append (current-directory) "/util/binary-search.sls"))])
          (set! document (file-node-document node))
          (construct-substitutions-for document)
          (let ([def-node (find-define-by-name (car (document-index-node-list document)) 'binary-search)])
            (set! target-node (cadr (index-node-children def-node))))))
      (type:interpret-result-list target-node))))

;; 7. hover API without type inference (binary-search)
(let ([workspace #f])
  (register-benchmark
    "hover binary-search (no ti)" 3
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #f)))
      (let* ([target-path (string-append (current-directory) "/util/binary-search.sls")]
             [uri (path->uri target-path)]
             [file-node (walk-file (workspace-file-node workspace) target-path)]
             [document (file-node-document file-node)]
             [root-node (car (document-index-node-list document))]
             [def-node (find-define-by-name root-node 'binary-search)]
             [name-node (define-node->name-node def-node)]
             [cursor-pos (document+bias->position-list document (index-node-start name-node))])
        (hover workspace (make-alist
                           'textDocument (make-alist 'uri uri)
                           'position (make-alist 'line (car cursor-pos) 'character (cadr cursor-pos))))
        '()))))

;; 8. hover API with type inference (assq-ref)
(let ([workspace #f])
  (register-benchmark
    "hover assq-ref (with ti)" 1
    (lambda ()
      (unless workspace
        (set! workspace (setup-workspace (current-directory) #t)))
      (let* ([target-path (string-append (current-directory) "/util/association.sls")]
             [uri (path->uri target-path)]
             [file-node (walk-file (workspace-file-node workspace) target-path)]
             [document (file-node-document file-node)]
             [root-node (car (document-index-node-list document))]
             [def-node (find-define-by-name root-node 'assq-ref)]
             [name-node (define-node->name-node def-node)]
             [cursor-pos (document+bias->position-list document (index-node-start name-node))])
        (hover workspace (make-alist
                           'textDocument (make-alist 'uri uri)
                           'position (make-alist 'line (car cursor-pos) 'character (cadr cursor-pos))))
        '()))))

;; ---------------------------------------------------------------------------
;; Run & report
;; ---------------------------------------------------------------------------

(define (pad str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append str (make-string (- width len) #\space)))))

(define (format-ms ms)
  (number->string ms))

(define (print-header)
  (display "\n")
  (display "========================================\n")
  (display "  Type Inference Benchmark Report\n")
  (display "========================================\n")
  (display (pad "Benchmark" 40))
  (display (pad "Real(ms)" 12))
  (display (pad "CPU(ms)" 12))
  (display (pad "GC(ms)" 12))
  (display (pad "Results" 10))
  (display "\n")
  (display (make-string 86 #\-))
  (display "\n"))

(define (print-result r)
  (display (pad (benchmark-result-name r) 40))
  (display (pad (format-ms (benchmark-result-real-ms r)) 12))
  (display (pad (format-ms (benchmark-result-cpu-ms r)) 12))
  (display (pad (format-ms (benchmark-result-gc-ms r)) 12))
  (display (pad (number->string (benchmark-result-result-count r)) 10))
  (display "\n"))

(define (write-json results path)
  (call-with-port
    (open-file-output-port path (file-options replace) 'block (make-transcoder (utf-8-codec)))
    (lambda (p)
      (display "{\n  \"timestamp\": \"" p)
      (display (date-and-time) p)
      (display "\",\n  \"results\": [\n" p)
      (let loop ([rs results])
        (unless (null? rs)
          (let ([r (car rs)])
            (display "    {\n" p)
            (display "      \"name\": \"" p)
            (display (benchmark-result-name r) p)
            (display "\",\n      \"real_ms\": " p)
            (display (benchmark-result-real-ms r) p)
            (display ",\n      \"cpu_ms\": " p)
            (display (benchmark-result-cpu-ms r) p)
            (display ",\n      \"gc_ms\": " p)
            (display (benchmark-result-gc-ms r) p)
            (display ",\n      \"result_count\": " p)
            (display (benchmark-result-result-count r) p)
            (display "\n    }" p))
          (if (null? (cdr rs))
              (display "\n" p)
              (display ",\n" p))
          (loop (cdr rs))))
      (display "  ]\n}\n" p))))

(define (main)
  (print-header)
  (let ([results (map (lambda (b)
                        (let ([name (car b)]
                              [runs (cadr b)]
                              [thunk (cddr b)])
                          (display "Running ")
                          (display name)
                          (display " ...")
                          (flush-output-port (current-output-port))
                          (let ([r (run-benchmark name runs thunk)])
                            (display " done\n")
                            (print-result r)
                            r)))
                      benchmarks)])
    (display (make-string 86 #\-))
    (display "\n")
    (let ([json-path "benchmark-type-inference.json"])
      (write-json results json-path)
      (display "\nWrote JSON results to ")
      (display json-path)
      (display "\n"))))

(main)
