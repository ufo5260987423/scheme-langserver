(library (scheme-langserver analysis type type-inferencer)
  (export type-inference-for)
  (import 
    (chezscheme)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type rules trivial)
    (scheme-langserver analysis type rules lambda)
    (scheme-langserver analysis type rules define)

    (scheme-langserver analysis type argument-checker)
    (scheme-langserver analysis type util))

;; We regard the indexes and references as a graph of existed variable and values. 
;;todo: first, construct type-tree, for procedure, it's like ((return-type-corresponding-index-node) ((first param-index-node)(second param-index-node)))
;; try to get result type by substitution
(define type-inference-for 
  (case-lambda 
    [(document) 
      (map 
        (lambda (index-node) 
          (let ([substitutions (private-construct-substitution-list document index-node)])
            (type-inference-for index-node substitutions)
            (map 
              (lambda (index-node) 
                (type-inference-for index-node substitutions))
              (index-node-children index-node))))
        (document-index-node-list document))]
    [(index-node substitutions)
      (let* ([tree (private-walk index-node substitutions)]
          [reified (dedupe (private-reify tree))])
        (index-node-actural-have-type-set! index-node reified))]))

(define (private-construct-substitution-list document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [children (index-node-children index-node)]
      [tmp-substitution-list 
        (let loop ([body 
              (list trivial-process 
              ; lambda-process define-process
              )]
            [result '()])
          (if (or (null? body) (not (zero? (length result))))
            result
            (loop (cdr body) (append result ((car body) document index-node result)))))]
      [children-substitution-list 
        (apply append (map (lambda (child) (private-construct-substitution-list document child)) children))])
    (append tmp-substitution-list children-substitution-list)))

(define (private-reify tree)
  (cond
    [(list? tree) (map private-reify tree)]
    [(index-node? tree) '(something? x)]
    [else tree]))

(define private-walk 
  (case-lambda
    [(index-node substitution-list) (private-walk index-node substitution-list)]
    [(index-node substitution-list path) 
      (if (contain? path index-node)
        '()
        (let ([targets 
              (map 
                cadr 
                (filter 
                  (lambda (target) (equal? index-node (car target))) 
                  substitution-list))]
            [current-path (append path `(,index-node))])
          (let loop ([dry-body (private-dry-tree targets)]
              [result targets])
            (if (null? dry-body)
              result
              (loop 
                (cdr dry-body) 
                (dedupe 
                  (apply append
                    (map 
                      (lambda (t) (private-substitute (car dry-body) t result))
                      (private-walk 
                        (car dry-body) 
                        substitution-list 
                        (append current-path `(,(car dry-body))))))))))))]))

(define (private-dry-tree target)
  (if (list? target)
    (dedupe (apply append (map private-dry-tree target)))
    (if (index-node? target)
      `(,target)
      '())))

(define (private-substitute origin target tree)
  (if (equal? tree origin)
    target
    (if (list? tree)
      (map (lambda (sub-tree) (private-substitute origin target sub-tree)) tree)
      tree)))
)