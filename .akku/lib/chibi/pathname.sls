#!r6rs
;; Akku.scm wrote this file based on "chibi-pathname-0.9.0/chibi/pathname.sld"

;;> A general, non-filesystem-specific pathname library.

(library
  (chibi pathname)
  (export
    path-strip-directory
    path-directory
    path-extension
    path-strip-extension
    path-replace-extension
    path-absolute?
    path-relative?
    path-strip-leading-parents
    path-relative-to
    path-resolve
    path-normalize
    make-path)
  (import
    (except (scheme base) string-map string-for-each)
    (chibi string))
  (include "pathname.scm"))
