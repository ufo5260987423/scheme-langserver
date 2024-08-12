#!r6rs
;; Akku.scm wrote this file based on "chibi-diff-0.9.1.3/chibi/diff.sld"

(library
  (chibi diff)
  (export
    lcs
    lcs-with-positions
    diff
    write-diff
    diff->string
    write-edits
    edits->string
    edits->string/color
    write-line-diffs
    write-line-diffs/color
    write-char-diffs
    write-char-diffs/color)
  (import
    (scheme base)
    (srfi :1)
    (chibi optional)
    (chibi term ansi))
  (define (port->list reader port)
    (let lp ((res '()))
      (let ((x (reader port)))
        (if (eof-object? x)
          (reverse res)
          (lp (cons x res))))))
  (include "diff.scm"))
