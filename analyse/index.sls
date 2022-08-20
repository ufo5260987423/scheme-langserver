(library (scheme-langserver analyse index)
  (export 
    init-index-node
    index-node?
    index-node-parent
    index-node-children
    index-node-children-set!
    index-node-start
    index-node-end
    index-node-datum/annotations

    source-file->annotation)
  (import 
    (chezscheme) 
    (scheme-langserver util io))

(define-record-type index-node
  (fields
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable parent)
    (mutable children)
    (immutable start)
    (immutable end)
    (immutable datum/annotations)
  ))

(define (init-index-node parent datum/annotations)
  (let* ([source (annotation-source datum/annotations)]
        [node (make-index-node parent '() (source-object-bfp source) (source-object-efp source) datum/annotations)]
        [expression (annotation-expression datum/annotations)])
    (index-node-children-set! 
      node 
      (if (list? expression)
        (map (lambda(e) (init-index-node node e)) expression)
        '()))
    node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define source-file->annotation
  (case-lambda
    ([path] (source-file->annotation (read-string path) path))
    ([source path]
      (let-values 
        ([(ann end-pos)
          (get-datum/annotations 
            (open-string-input-port source) 
            (make-source-file-descriptor path (open-file-input-port path)) 0)])
        ann))))
)