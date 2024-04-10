(library (scheme-langserver analysis identifier rules case-lambda)
  (export case-lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules lambda)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; parameter 
(define (case-lambda-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_(dummy0 ...) dummy1 ... ) 
          (let loop ([rest (cdr (index-node-children index-node))])
            (if (not (null? rest))
              (let* ([identifier-index-node-grand-parent (car rest)]
                  [grand-parent-expression (annotation-stripped (index-node-datum/annotations identifier-index-node-grand-parent))])
                (match grand-parent-expression 
                  ; Because case-lambda has many clauses, and some maybe don't contain any parameters
                  [(() body ...) (loop (cdr rest))]
                  [((param-identifier **1) body ...)
                    (let* ([identifier-index-node-parent (car (index-node-children identifier-index-node-grand-parent))])
                      (let param-loop ([exclude '()] [param-identifier-index-node-list (index-node-children identifier-index-node-parent)])
                        (if (null? param-identifier-index-node-list)
                          (loop (cdr rest))
                          (param-loop 
                            (append exclude (parameter-process index-node (car param-identifier-index-node-list) identifier-index-node-grand-parent exclude document)) 
                            (cdr param-identifier-index-node-list)))))]
                  [else '()]
                ))))]
        [else '()])
      (except c
        [else '()]))))
)