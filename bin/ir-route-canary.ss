#!chezscheme
;; Canary for the route-3 controlled parallel phase (ir-auto-route): analyze
;; this repository with init-workspace and print a compact metric line, so two
;; runs (SLSCHEME_IR_ROUTE unset vs set) can be diffed. Metrics: wall time,
;; IR-route stats, aggregate reference/diagnosis counts over all documents.
;;
;;   source .akku/bin/activate
;;   scheme --script bin/ir-route-canary.ss              # route 2 mode
;;   SLSCHEME_IR_ROUTE=1 scheme --script bin/ir-route-canary.ss   # IR mode
(import (chezscheme)
  ;deliberately NO (ufo-match): dogfood the v2 load fallback (workspace
  ;library-node / CHEZSCHEMELIBDIRS locate + load) instead of the
  ;interaction fast path.
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier ir-auto-route)
  (scheme-langserver analysis workspace))

(define (fold-documents proc init root-file-node)
  (let loop ([node root-file-node] [acc init])
    (let* ([doc (file-node-document node)]
        [acc2 (if (and doc (not (null? doc))) (proc doc acc) acc)])
      (let child-loop ([children (file-node-children node)] [a acc2])
        (if (null? children)
          a
          (child-loop (cdr children) (loop (car children) a)))))))

(define (dump-diagnoses! workspace path)
  (let ([out (open-output-file path 'replace)])
    (fold-documents
      (lambda (doc acc)
        (for-each
          (lambda (diag)
            ;diag: (start end severity message source type)
            (fprintf out "~a ~a ~a ~a ~a~%"
              (document-uri doc)
              (list-ref diag 0) (list-ref diag 1) (list-ref diag 2)
              (list-ref diag 3)))
          (sort (lambda (a b) (string<? (format "~a" a) (format "~a" b)))
            (document-diagnoses doc))))
      (void)
      (workspace-file-node workspace))
    (close-port out)))

(define (main)
  (let* ([root (current-directory)]
      [t0 (current-time 'time-monotonic)]
      [workspace (init-workspace root 'akku 'r6rs #f #f)]
      [t1 (current-time 'time-monotonic)]
      [init-ms (/ (time-nanosecond (time-difference t1 t0)) 1000000.0)]
      [stats (ir-auto-route:stats)]
      [_ (let ([dump (getenv "DIAG_DUMP")])
          (when dump (dump-diagnoses! workspace dump)))]
      [totals
        (fold-documents
          (lambda (doc acc)
            (list
              (+ (car acc) (length (document-ordered-reference-list doc)))
              (+ (cadr acc) (length (document-diagnoses doc)))
              (+ (caddr acc) 1)))
          '(0 0 0)
          (workspace-file-node workspace))])
    (printf "init-ms=~,0f ir-stats=(~a ~a ~,1f) refs=~a diagnoses=~a documents=~a~%"
      init-ms
      (car stats) (cadr stats) (caddr stats)
      (car totals) (cadr totals) (caddr totals))))

(main)
