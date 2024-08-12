#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (srfi private platform-features)
  (export
    expand-time-features
    run-time-features)
  (import
    (rnrs)
    (only (core) architecture-feature)
    (srfi private OS-id-features))

  (define (expand-time-features)
    '(ypsilon))

  (define (run-time-features)
    (OS-id-features
     (guard (exn ((assertion-violation? exn)
                  ;; Fallback for releases before v2.0.1
                  (architecture-feature 'operating-system)))
       (architecture-feature 'sysname))
     '(("linux" linux posix)
       ("solaris" solaris posix)
       ("darwin" darwin posix)
       ("bsd" bsd)
       ("freebsd" freebsd posix)
       ("openbsd" openbsd posix)
       ("windows" windows))))
)
