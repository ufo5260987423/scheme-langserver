#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
(import (chezscheme))

(library-directories '(("debug-trace" . ".akku/libobj") (".akku/lib" . ".akku/libobj")))
(import (ufo-match))

(display "==== EXPANDING: (match x [(? string? path) path]) ====\n\n")
(let ([expanded (expand '(match x [(? string? path) path]))])
  (display "\n==== FINAL EXPANDED FORM ====\n")
  (pretty-print (syntax->datum expanded)))
(display "\n==== DONE ====\n")
