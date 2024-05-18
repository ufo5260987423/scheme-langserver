(library (scheme-langserver analysis local-expand)
  (export 
    local-expand
    unwrap-equal?
    unwrap-symbol)
  (import 
    (chezscheme) 
    (ufo-match)
    (only (srfi :13 strings) string-prefix? string-drop)
    
    (scheme-langserver virtual-file-system document)

    (scheme-langserver util path)
    (scheme-langserver util dedupe)

    (scheme-langserver analysis util)
    (scheme-langserver analysis dependency shrinker)
    (scheme-langserver analysis dependency file-linkage)
    (scheme-langserver analysis dependency rules library-import)
    (scheme-langserver analysis identifier reference))
  
(define (local-expand to-eval document root-library-node file-linkage)
  (let* ([uri (document-uri document)]
      [path (uri->path uri)]
      [get-reference-paths (get-reference-path-from file-linkage path)]
      [patches (apply append (shrink-paths file-linkage get-reference-paths))]
      [to-load (append '(begin ) (map (lambda (p) `(load ,p)) patches))]
      [imported-libraries 
        (dedupe (apply append 
          (map (lambda (index-node) (library-import-process index-node))
            (document-index-node-list document))))]
      [to-import (map (lambda (l) `(import ,l)) imported-libraries)]
      [target `(expand ',to-eval)])
    (private-simplify-gensyms (eval `(,@to-load ,@to-import ,target)))))

(define (unwrap-equal? a b)
  (equal? (unwrap-symbol a) (unwrap-symbol b)))

(define (unwrap-symbol expression)
  (if (local-expand-escape-symbol? expression)
    (string->symbol (string-drop (symbol->string expression) 7))
    expression))

(define private-prefix "escape-")

(define (local-expand-escape-symbol? expression)
  (string-prefix? private-prefix (symbol->string expression)))

(define (private-simplify-gensyms expression)
  (cond 
    [(gensym? expression)  (string->symbol (string-append private-prefix (gensym->unique-string expression) "-" (symbol->string expression)))]
    [(list? expression) (map private-simplify-gensyms expression)]
    [(vector? expression) (vector-map private-simplify-gensyms expression)]
    [(pair? expression) (cons (private-simplify-gensyms (car expression)) (private-simplify-gensyms (cdr expression)))]
    [(symbol? expression) 
      (let ([s (symbol->string expression)])
        (if (string-prefix? private-prefix s)
          (string->symbol (string-append private-prefix s))
          expression))]
    [else expression]))
)