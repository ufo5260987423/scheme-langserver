(library (scheme-langserver util environment)
    (export 
      windows?)
    (import 
      (chezscheme) 
      (only (srfi :13 strings) string-suffix?))

; (define arch-pairs
;   '(("a6"    . amd64)
;     ("arm32" . arm32)
;     ("i3"    . i386)
;     ("ppc32" . ppc32)))

; (define os-pairs
;   '(("fb"  . freebsd)
;     ("le"  . linux)
;     ("nb"  . netbsd)
;     ("nt"  . windows)
;     ("ob"  . openbsd)
;     ("osx" . macos)
;     ("qnx" . qnx)
;     ("s2"  . solaris)))

; (machine-type) =>ta6le t:threaded a6:amd64 le:linux

(define (windows?)
  (string-suffix? "nt" (symbol->string (machine-type))))
)