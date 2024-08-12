#!r6rs
;; Akku.scm wrote this file based on "chibi-diff-0.9.1.3/chibi/diff-test.sld"

(library
  (chibi diff-test)
  (export run-tests)
  (import
    (scheme base)
    (chibi diff)
    (scheme write))
  (define-syntax test
    (syntax-rules ()
      ((test expect expr) (test 'expr expect expr))
      ((test name expect expr)
       (guard (exn (else (display "!\nERROR: ")
                         (write name)
                         (newline)
                         (write exn)
                         (newline)))
              (let* ((res expr) (pass? (equal? expect expr)))
                (display (if pass? "." "x"))
                (cond ((not pass?)
                       (display "\nFAIL: ")
                       (write name)
                       (newline))))))))
  (define (test-begin name) (display name))
  (define (test-end) (newline))
  (define (run-tests)
    (test-begin "diff")
    (test '((#\A 1 0) (#\C 2 2))
          (lcs-with-positions
            '(#\G #\A #\C)
            '(#\A #\G #\C #\A #\T)))
    (test '(#\A #\C)
          (lcs '(#\G #\A #\C) '(#\A #\G #\C #\A #\T)))
    (test '((#\G #\A #\C)
            (#\A #\G #\C #\A #\T)
            ((#\A 1 0) (#\C 2 2)))
          (diff "GAC" "AGCAT" read-char))
    (let ((d (diff "GAC" "AGCAT" read-char)))
      (test " »G« AC"
            (edits->string (car d) (car (cddr d)) 1))
      (test "A «G» C «AT» "
            (edits->string (cadr d) (car (cddr d)) 2))
      (test "\x1b;[31mG\x1b;[39mAC"
            (edits->string/color (car d) (car (cddr d)) 1))
      (test "A\x1b;[32mG\x1b;[39mC\x1b;[32mAT\x1b;[39m"
            (edits->string/color (cadr d) (car (cddr d)) 2)))
    (test-end)))
