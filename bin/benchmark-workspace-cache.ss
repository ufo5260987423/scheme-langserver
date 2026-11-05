#!/usr/bin/env scheme-script
;; Fixture benchmark table for the workspace FASL cache: cold init, cached
;; init, and cache-save timings per fixture.
;; Run: source .akku/bin/activate && scheme --script bin/benchmark-workspace-cache.ss [mode]
;;   mode = fixtures (default): simple-lib + synthetic-200 + repo (macro off)
;;   mode = repo-on: repo with ir-auto-route enabled (run in a FRESH process)
#!r6rs

(import
  (chezscheme)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis package-manager file-filter)
  (scheme-langserver analysis identifier expander ir-auto-route))

(define (current-real-ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t))
       (div (time-nanosecond t) 1000000))))

(define (time-it thunk)
  (let ([t0 (current-real-ms)])
    (thunk)
    (- (current-real-ms) t0)))

(define (min-of n thunk)
  ;; thunk is stateful (cache dirs etc.), so runs must be self-contained:
  ;; thunk takes run index; returns ms
  (let loop ([i 0] [best #f])
    (if (= i n)
      best
      (let ([ms (thunk i)])
        (loop (+ i 1) (if (or (not best) (< ms best)) ms best))))))

(define (benchmark label path file-filter runs macro-on?)
  (printf "~a (~a, macro ~a):~n" label path (if macro-on? "on" "off"))
  (let ([cache (string-append "/tmp/sls-cache-bench-" label)])
    (let ([cold
           (min-of runs
             (lambda (i)
               (when macro-on? (ir-auto-route:enabled? #t))
               (delete-tree-if-exists cache)
               (time-it (lambda () (init-workspace path file-filter 'r6rs #f #f)))))])
      (printf "  cold init-workspace:  ~6d ms~n" cold))
    ;; cache now exists from last run
    (let ([warm
           (min-of runs
             (lambda (i)
               (when macro-on? (ir-auto-route:enabled? #t))
               (time-it (lambda () (init-workspace path file-filter 'r6rs #f #f cache)))))])
      (printf "  cached init-workspace: ~6d ms~n" warm))
    (let ([save
           (min-of runs
             (lambda (i)
               (when macro-on? (ir-auto-route:enabled? #t))
               (let ([ws (init-workspace path file-filter 'r6rs #f #f)])
                 (time-it (lambda () (save-workspace-cache-for! ws cache 'r6rs #f #f))))))])
      (printf "  cache save:            ~6d ms~n" save))))

(define (delete-tree-if-exists p)
  (when (file-directory? p)
    (for-each
      (lambda (f)
        (let ([child (string-append p "/" f)])
          (if (file-directory? child)
            (delete-tree-if-exists child)
            (delete-file child))))
      (directory-list p))
    (delete-directory p)))

(define (main)
  (let ([mode (if (null? (command-line-arguments)) "fixtures" (car (command-line-arguments)))])
    (cond
      [(equal? mode "fixtures")
       (benchmark "simple-lib"
         (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")
         'txt 3 #f)
       (benchmark "synthetic-200" "/tmp/sls-fixture-200" 'txt 3 #f)
       (benchmark "repo" (current-directory) 'akku 3 #f)]
      [(equal? mode "repo-on")
       (benchmark "repo-on" (current-directory) 'akku 3 #t)]
      [else (error 'main "unknown mode ~a" mode)])))

(main)
