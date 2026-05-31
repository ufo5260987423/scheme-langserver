(library (scheme-langserver util test)
  (export 
    annotation-stripped-expression
    find-index-node-recursive
    find-define-with-params
    find-define-by-name
    define-node->name-node
    find-let-node
    find-named-let
    find-binding-node
    let-body-nodes
    find-symbol-in-body)
  (import 
    (chezscheme)
    (scheme-langserver virtual-file-system index-node))

(define (annotation-stripped-expression index-node)
  (annotation-stripped (index-node-datum/annotations index-node)))

(define (find-index-node-recursive predicate index-node)
  (if (predicate index-node)
      index-node
      (let loop ([children (index-node-children index-node)])
        (if (null? children)
            #f
            (or (find-index-node-recursive predicate (car children))
                (loop (cdr children)))))))

(define (find-define-with-params root-node name)
  (find-index-node-recursive
    (lambda (node)
      (let ([expr (annotation-stripped-expression node)])
        (and (list? expr) (not (null? expr))
             (eq? 'define (car expr))
             (list? (cadr expr))
             (eq? name (car (cadr expr))))))
    root-node))

;; Matches both (define (name ...) ...) and (define name ...)
(define (find-define-by-name root-node name)
  (find-index-node-recursive
    (lambda (node)
      (let ([expr (annotation-stripped-expression node)])
        (and (list? expr) (not (null? expr))
             (eq? 'define (car expr))
             (or (and (list? (cadr expr)) (eq? name (car (cadr expr))))
                 (and (symbol? (cadr expr)) (eq? name (cadr expr)))))))
    root-node))

;; Extract the name symbol-node from either
;;   (define (name ...) ...) or (define name ...)
(define (define-node->name-node define-node)
  (let ([expr (annotation-stripped-expression define-node)])
    (if (and (list? expr) (not (null? expr)) (eq? 'define (car expr)))
        (let ([name-part (cadr expr)])
          (if (list? name-part)
              ;; (define (name ...) ...) -> name is first child of params list
              (car (index-node-children (cadr (index-node-children define-node))))
              ;; (define name ...) -> name is second child of define
              (cadr (index-node-children define-node))))
        #f)))

(define (find-let-node root-node)
  (find-index-node-recursive
    (lambda (node)
      (let ([expr (annotation-stripped-expression node)])
        (and (list? expr) (not (null? expr))
             (eq? 'let (car expr))
             (>= (length expr) 2)
             (list? (cadr expr)))))
    root-node))

(define (find-named-let root-node name)
  (find-index-node-recursive
    (lambda (node)
      (let ([expr (annotation-stripped-expression node)])
        (and (list? expr) (not (null? expr))
             (>= (length expr) 3)
             (eq? 'let (car expr))
             (eq? name (cadr expr)))))
    root-node))

(define (find-binding-node let-node binding-name)
  (let* ([children (index-node-children let-node)]
         [bindings-list-node 
           (if (and (>= (length children) 3)
                    (symbol? (annotation-stripped-expression (cadr children))))
               (caddr children)
               (cadr children))])
    (find (lambda (binding-node)
            (let ([expr (annotation-stripped-expression binding-node)])
              (and (list? expr) (not (null? expr)) (eq? binding-name (car expr)))))
          (index-node-children bindings-list-node))))

(define (let-body-nodes let-node)
  (let ([children (index-node-children let-node)])
    (if (and (>= (length children) 3)
             (symbol? (annotation-stripped-expression (cadr children))))
        (list-tail children 3)
        (list-tail children 2))))

(define (find-symbol-in-body let-node symbol-name)
  (let loop ([body (let-body-nodes let-node)])
    (if (null? body)
        #f
        (or (find-index-node-recursive
              (lambda (n) (eq? symbol-name (annotation-stripped-expression n)))
              (car body))
            (loop (cdr body))))))
)
