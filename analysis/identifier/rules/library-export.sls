(library (scheme-langserver analysis identifier rules library-export)
  (export 
    export-process
    export-process-r7rs)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; pointer 

; NOTE: the difference between variable and pointer is 
; usually variables store the result of tailed s-expression
; like (let ([A a])...) and A is a variable recalled in the fowlling body
; but pointers manipulate the result of previous s-expression
; like (rename (a A)) and A is a pointer recalled outsize this body 
(define (export-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ library-identifiers . clauses)
      (map 
        (lambda (child-node) (match-export index-node root-file-node document library-identifiers child-node))
        clauses)]
    [else '()])
  index-node)

(define export-process-r7rs export-process)

(define (match-export initialization-index-node root-file-node document library-identifiers index-node)
  (match-index-node index-node
    [('export . clauses)
      (map 
        (lambda (child-node) (match-clause initialization-index-node root-file-node document library-identifiers child-node)) 
        clauses)]
    [else '()]))

(define (match-clause initialization-index-node root-file-node document library-identifiers index-node)
  (match-index-node index-node
    [('rename . rename-pairs)
      (fold-left
        (lambda (result current-pair)
          (let* ([current-children (index-node-children current-pair)]
              [internal-index-node (car current-children)]
              [external-index-node (cadr current-children)]
              [references
                (find-available-references-for 
                  document
                  internal-index-node 
                  (index-node-expression internal-index-node))])
            (append-references-into-ordered-references-for document external-index-node references)

            (index-node-references-export-to-other-node-set! 
              external-index-node
              (append 
                (index-node-references-export-to-other-node external-index-node)
                `(,(make-identifier-reference
                    (index-node-expression external-index-node)
                    document
                    external-index-node
                    initialization-index-node 
                    (index-node-expression library-identifiers)
                    'pointer
                    references
                    (apply append (map identifier-reference-type-expressions references))))))
            `(,@result ,external-index-node)))
        '()
        rename-pairs)]
    [(? index-node-symbol? identifier)
      (let* ([references (find-available-references-for document identifier (index-node-expression identifier))]
          [reference-count (length references)])
        (index-node-references-export-to-other-node-set! 
          identifier
          (append 
            (index-node-references-export-to-other-node identifier)
            (if (zero? reference-count)
        ;; in srfi 13, library file using a self-made include/revolve procedure
        ;; and in this case, replace '() with a special 
              `(,(make-identifier-reference
                (index-node-expression identifier)
                document
                identifier
                initialization-index-node 
                (index-node-expression library-identifiers)
                'pointer
                references
                (apply append (map identifier-reference-type-expressions references))))
              references))))]
    [else '()]))
)
