(library (scheme-langserver analysis identifier rules library-export)
  (export export-process)
  (import 
    (chezscheme) 
    (ufo-match)

    ; (scheme-langserver util try)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; pointer 

; NOTE: the difference between variable and pointer is 
; usually variables store the result of tailed s-expression
; like (let ([A a])...) and A is a variable recalled in the fowlling body
; but pointers manipulate the result of previous s-expression
; like (rename (a A)) and A is a pointer recalled outsize this body 
(define (export-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (library-identifiers **1) fuzzy **1 ) 
        (map 
          (lambda (child-node) (match-export index-node root-file-node document library-identifiers child-node))
          (index-node-children index-node))]
      ; [('define-library (library-identifiers **1) _ **1 ) 
      ;   (map 
      ;     (lambda (child-node) (match-export index-node root-file-node document library-identifiers child-node))
      ;     (index-node-children index-node))]
      [else '()])
    index-node))

(define (match-export initialization-index-node root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('export dummy **1 ) 
        (map 
          (lambda (child-node) (match-clause initialization-index-node root-file-node document library-identifiers child-node)) 
          (cdr (index-node-children index-node)))]
      [else '()])))

(define (match-clause initialization-index-node root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('rename (internal-names external-names) **1) 
        (fold-left
          (lambda (result current-item)
            (let* ([current-children (index-node-children current-item)]
                [internal-index-node (car current-children)]
                [external-index-node (cadr current-children)]
                [references
                  (find-available-references-for 
                    document
                    internal-index-node 
                    (annotation-stripped (index-node-datum/annotations internal-index-node)))])
              (index-node-references-import-in-this-node-set! 
                external-index-node
                (sort-identifier-references
                  (append (index-node-references-import-in-this-node external-index-node) references)))

              (index-node-references-export-to-other-node-set! 
                external-index-node
                (append 
                  (index-node-references-export-to-other-node external-index-node)
                  `(,(make-identifier-reference
                      (annotation-stripped (index-node-datum/annotations external-index-node))
                      document
                      external-index-node
                      initialization-index-node 
                      library-identifiers
                      'pointer
                      references
                      (apply append (map identifier-reference-type-expressions references))))))
              `(,@result ,external-index-node)))
          '()
          (cdr (index-node-children index-node)))]
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
                  initialization-index-node 
                  library-identifiers
                  'pointer
                  references
                  (apply append (map identifier-reference-type-expressions references))))
                references))))]
      [else '()])))
)