(library (scheme-langserver analysis syntax util)
  (export 
    pattern->symbols
    literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node
    route+node->index-node-pair:old+new)
  (import 
    (chezscheme)
    (ufo-match)
    (ufo-try)
    (scheme-langserver util contain)
    (scheme-langserver util dedupe)

    (scheme-langserver virtual-file-system index-node))

(define (route+node->index-node-pair:old+new route new-index-node)
  (let* ([children (index-node-children new-index-node)])
    (cond
      [(index-node? route) `((,route . ,new-index-node))]
      [(null? route) '()]
      [(list? route)
        (apply append 
          (map (lambda (l r) (route+node->index-node-pair:old+new i )) 
            route (index-node-children new-index-node)))]
      [(pair? route)
        (apply append 
          (route+node->index-node-pair:old+new (car route) (car (index-node-children new-index-node)))
          (route+node->index-node-pair:old+new (cdr route) (cdr (index-node-children new-index-node))))])))

(define (pattern->symbols literals pattern-sexpression)
  (cond 
    [(equal? pattern-sexpression '...) '()]
    [(list? pattern-sexpression) 
      (fold-left 
        (lambda (left right)
          (dedupe (append left (pattern->symbols literals right))))
        '()
        pattern-sexpression)]
    [(contain? literals pattern-sexpression) '()]
    [else `(,pattern-sexpression)]))

(define (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node literals pattern callee-index-node/s)
  (cond 
    [(and (null? pattern) (null? callee-index-node/s)) '()]
    [(null? pattern) (raise 'fail-to-match-callee)]

    [(and (symbol? pattern) (index-node? callee-index-node/s)) 
      (if (contain? literals pattern)
        '()
        `((,pattern . ,callee-index-node/s)))]
    [(symbol? pattern) (raise 'pattern-ellipsed?)]

    [(index-node? callee-index-node/s) 
      (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node 
        literals 
        pattern 
        (index-node-children callee-index-node/s))]

    [(>= (length pattern) 2)
      (cond 
        [(equal? '... (cadr pattern)) 
          (let ([n (length (private:match-head-children keywords pattern expression 2))])
            (append 
              (fold-left
                (lambda (left right)
                  (append left
                    (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node
                      literals
                      (car pattern)
                      right)))
                '()
                (list-head callee-index-node/s n))
              (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node
                literals
                (cddr pattern)
                (list-tail callee-index-node/s n))))]
        [else 
          (append 
            (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node
              literals
              (car pattern)
              (car callee-index-node/s))
            (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node
              literals
              (cdr pattern)
              (cdr callee-index-node/s)))])]

    [(> (length callee-index-node/s) 1) 
      (raise 'can-not-match)]
    [else  
      (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node 
        literals 
        (car pattern) 
        (car callee-index-node/s))]))

(define (private:match-head-children keywords pattern callee-expression number)
  (eval 
    `(syntax-case ',callee-expression ,keywords 
      [,pattern ,(list-head pattern number)])))
)