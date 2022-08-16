(library (scheme-langserver analyse index)
  (export init-index-node)
  (import 
    (chezscheme) 
    (scheme-langserver uti io))

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

; (define (init-index-node document parent tree)
;   (let ([text (document-text document)]
;       [node (make-index-node document parent '())]
;       )
;       )
;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; (define e
    ; (let-values (
    ;     [(a b ) 
    ;       (get-datum/annotations (open-string-input-port "(quote (1 2 3))") 
    ;       (make-source-file-descriptor "1.ss" (open-file-input-port "/home/ufo/1.ss")) 0)])
    ;     a
    ; ))
    ; (annotation-expression e)
(define (source-file->annotation path)
  (let-values 
    ([(ann end-pos)
      (get-datum/annotations (read-string path) (make-source-file-descriptor path (open-file-input-port path) 0))])
    ann))
)