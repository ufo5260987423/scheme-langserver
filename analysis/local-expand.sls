(library (scheme-langserver analysis local-expand)
  (export 
    local-expand)
  (import 
    (chezscheme) 
    (ufo-match)
    
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

(define (private-simplify-gensyms expression)
  (cond 
    [(gensym? expression)  (string->symbol (string-append (gensym->unique-string expression) "-" (symbol->string expression)))]
    [(list? expression) (map private-simplify-gensyms expression)]
    [(vector? expression) (vector-map private-simplify-gensyms expression)]
    [(pair? expression) (cons (private-simplify-gensyms (car expression)) (private-simplify-gensyms (cdr expression)))]
    [else expression]))
)