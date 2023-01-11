(library (scheme-langserver analysis identifier rules library-export)
  (export export-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; pointer 

; NOTE: the difference between variable and pointer is 
; usually variables store the result of tailed s-expression
; like (let ([A a])...) and A is a variable recalled in the fowlling body
; but pointers manipulate the result of previous s-expression
; like (rename (a A)) and A is a pointer recalled outsize this body 
(define (export-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('library (library-identifiers **1) _ **1 ) 
        (map 
          (lambda (child-node) (match-export root-file-node document library-identifiers child-node))
          (index-node-children index-node))]
      [else '()])
    index-node))

(define (match-export root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('export dummy **1 ) 
        (map 
          (lambda (child-node) (match-clause root-file-node document library-identifiers child-node)) 
          (cdr (index-node-children index-node)))]
      [else '()])))

(define (match-clause root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('rename (internal-names external-names) **1) 
        (let loop ([children-index-nodes (cdr (index-node-children index-node))]
                [internal-index-node (car (index-node-children (cadr (index-node-children index-node))))]
                [external-index-node (cadr (index-node-children (cadr (index-node-children index-node))))])

          (index-node-references-import-in-this-node-set! 
            external-index-node
            (append 
              (index-node-references-import-in-this-node external-index-node)
              (find-available-references-for 
                document
                internal-index-node 
                (annotation-stripped (index-node-datum/annotations internal-index-node)))))

          (index-node-references-export-to-other-node-set! 
            external-index-node
            (append 
              (index-node-references-export-to-other-node external-index-node)
              `(,(make-identifier-reference
                  (annotation-stripped (index-node-datum/annotations external-index-node))
                  document
                  external-index-node
                  library-identifiers
                  'pointer
                  '()))))

          (if (not (null? (cdr children-index-nodes)))
            (loop 
              (cdr children-index-nodes)
              (car (index-node-children (cadr children-index-nodes)))
              (cadr (index-node-children (cadr children-index-nodes))))))]
      [identifier
        (let* ([references (find-available-references-for document index-node identifier)]
            [reference-count (length references)])
          (index-node-references-export-to-other-node-set! 
            index-node
            (append 
              (index-node-references-export-to-other-node index-node)
              (if (zero? reference-count)
          ;; in srfi 13, library file using a self-made include/revolve procedure
          ;; and in this case, replace '() with a special 
                `(,(make-identifier-reference
                  expression
                  document
                  index-node
                  library-identifiers
                  'pointer
                  '()))
                references))))]
      [else '()])))
)