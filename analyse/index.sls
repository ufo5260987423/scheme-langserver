(library (scheme-langserver analyse index)
  (export init-index-node)
  (import 
    (chezscheme) 
    (scheme-langserver util io))

(define-record-type index-node
  (fileds
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable parent)
    (immutable children)
    (immutable start)
    (immutable end)
    (immutable datum/annotations)
  ))

(define (init-index-node parent datum/annotations)
  (let* ([source (annotation-source datum/annotations)]
        [node (make-index-node parent '() (source-object-bfp source) (source-object-efp source) datum/annotations)]
        [expression (annotation-epression datum/annotations)]
        [children (if (annotation? expression))
                    (map (lambda (e) (init-index-node node e)) (annotation-expression datum-annotations))
                    '()])
    (index-node-children-set! node children)
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