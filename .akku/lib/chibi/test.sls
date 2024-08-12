#!r6rs
;; Akku.scm wrote this file based on "chibi-test-0.9.0/chibi/test.sld"

(library
  (chibi test)
  (export
    test
    test-equal
    test-error
    test-assert
    test-not
    test-values
    test-group
    current-test-group
    test-begin
    test-end
    test-syntax-error
    test-propagate-info
    test-run
    test-exit
    test-equal?
    test-get-name!
    test-group-name
    test-group-ref
    test-group-set!
    test-group-inc!
    test-group-push!
    current-test-verbosity
    current-test-applier
    current-test-skipper
    current-test-reporter
    current-test-group-reporter
    test-failure-count
    current-test-epsilon
    current-test-comparator
    current-test-filters
    current-test-removers
    current-test-group-filters
    current-test-group-removers
    current-column-width)
  (import
    (scheme base)
    (scheme write)
    (scheme complex)
    (scheme process-context)
    (scheme time)
    (chibi diff)
    (chibi term ansi))
  (define (pair-source x) #f)
  (define print-exception write)
  (include "test.scm"))
