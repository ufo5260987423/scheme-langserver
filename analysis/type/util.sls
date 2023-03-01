(library (scheme-langserver analysis type util)
  (export 
    lambda?
    construct-lambda
    construct-type-expression-with-meta
    collect-reference-should-have-type)
  (import 
    (chezscheme)
    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta))

(define-syntax construct-lambda 
  (syntax-rules ()
    [(_ body) (eval `(lambda(x) ,body))]))

(define (lambda? body)
  (= 2 (length body)))

(define (collect-reference-should-have-type identifier index-node)
  (if (null? (index-node-children index-node))
    (if (equal? identifier (annotation-stripped (index-node-datum/annotations index-node)))
      (index-node-should-have-type index-node)
      '())
    (let ([maybe-result 
          (dedupe 
            (apply append 
              (map 
                (lambda(x) 
                  (collect-reference-should-have-type identifier x)) 
                (index-node-children index-node))))])
      (if (< 1 (length maybe-result))
        '(or ,@maybe-result)
        maybe-result))))

(define (construct-type-expression-with-meta meta-identifier)
  (if (list? meta-identifier)
    (map construct-type-expression-with-meta meta-identifier)
    (let* ([target-meta (find-meta '(rnrs))]
        [target-identifier (find (lambda(x) (equal? (identifier-reference-identifier x) meta-identifier)) target-meta)])
      (if target-identifier target-identifier meta-identifier))))

(define (expand-or type-expression)
  (if (list? type-expression)
    (if (equal? 'or (car type-expression))
      (apply append 
        (filter (lambda(x) (not (null? x)))
          (map expand-or (cdr type-expression))))
      `(,type-expression))
    '()))

)