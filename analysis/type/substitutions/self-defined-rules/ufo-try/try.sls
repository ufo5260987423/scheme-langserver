(library (scheme-langserver analysis type substitutions self-defined-rules ufo-try try)
  (export try-process)
  (import 
    (chezscheme) 
    (ufo-match)
    (ufo-try)

    (scheme-langserver analysis util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (try-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ something ... ('except (? symbol? c) branch **1))
          (let* ([children (index-node-children index-node)]
              [except-index-node (car (reverse children))]
              [except-children (index-node-children except-index-node)]
              [something-end-index-node (cadr (reverse children))]
              [branch-index-nodes (cddr except-children)])
            (extend-index-node-substitution-list index-node something-end-index-node)
            (map 
              (lambda (i) (private:branch i index-node))
              branch-index-nodes))]
        [else '()])
      (except c
        [else '()]))))

(define (private:branch branch-index-node try-index-node)
  (let ([c (reverse (index-node-children branch-index-node))])
    (if (> (length c) 1)
      (extend-index-node-substitution-list try-index-node (car c))
      '())))
)