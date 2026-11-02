#!/usr/bin/env scheme-script
;; Fresh-process cached-startup timing: how long does init-workspace take
;; when a valid cache exists? Run EACH config in a fresh process:
;;   scheme --script bin/benchmark-cached-init.ss <path> <filter> <cache-dir>
#!r6rs

(import
  (chezscheme)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis workspace-cache)
  (scheme-langserver analysis package-manager file-filter))

(define (current-real-ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t))
       (div (time-nanosecond t) 1000000))))

(define args (command-line-arguments))
(define path (car args))
(define file-filter (string->symbol (cadr args)))
(define cache (caddr args))

;; make sure a cache exists (cold-init + save first if missing)
(unless (workspace-cache-available? cache)
  (let ([ws (init-workspace path file-filter 'r6rs #f #f)])
    (save-workspace-cache-for! ws cache 'r6rs #f #f)))

(collect)
(let ([t0 (current-real-ms)])
  (init-workspace path file-filter 'r6rs #f #f cache)
  (printf "cached init-workspace (fresh process): ~d ms~n" (- (current-real-ms) t0)))
