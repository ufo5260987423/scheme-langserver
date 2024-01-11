(library (scheme-langserver analysis type substitutions generator)
  (export 
    construct-substitution-list-for)
  (import 
    (chezscheme)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type substitutions rules if)
    (scheme-langserver analysis type substitutions rules do)
    (scheme-langserver analysis type substitutions rules let)
    (scheme-langserver analysis type substitutions rules lambda)
    (scheme-langserver analysis type substitutions rules record)
    (scheme-langserver analysis type substitutions rules trivial)
    (scheme-langserver analysis type substitutions rules define)
    (scheme-langserver analysis type substitutions rules application)
    (scheme-langserver analysis type substitutions util))

(define (construct-substitution-list-for document)
  (document-substitution-list-set! 
    document 
    (dedupe 
      (sort 
        substitution-compare
        (apply 
          append
          (map 
            (lambda (index-node) (private-construct-substitution-list document index-node '() #f #f))
            (document-index-node-list document)))))))

(define (private-construct-substitution-list document index-node base-substitution-list allow-unquote? quoted?)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)]
      [variable (index-node-variable index-node)])
    ; (pretty-print 'generate)
    ; (pretty-print expression)
    ; (pretty-print (document-uri document))
    ; (pretty-print variable)
    (cond
      [(null? children) 
        ; (pretty-print 'cao5)
        (append base-substitution-list 
          (trivial-process document index-node variable expression base-substitution-list allow-unquote? quoted?))]
      ;here, must be a list or vector
      [(and (not quoted?) (quasiquote? index-node document))
        ; (pretty-print 'cao6)
        (private-construct-substitution-list 
          document 
          (car children) 
          `(,@base-substitution-list 
            (,variable = ,(index-node-variable (car children))))
          #t 
          #t)]
      [(and (not quoted?) (quote? index-node document))
        ; (pretty-print 'cao0)
        (private-construct-substitution-list 
          document 
          (car children) 
          `(,@base-substitution-list 
            (,variable = ,(index-node-variable (car children))))
          #f 
          #t)]
      [(not quoted?)
        ; (pretty-print 'cao1)
        (let ([children-substitution-list
              (fold-left 
                (lambda (previous-substitutions child-index-node)
                  (private-construct-substitution-list document child-index-node previous-substitutions #f #f))
                base-substitution-list
                children)])
          (fold-left
            (lambda (previous-substitutions proc)
              ; (pretty-print 'proc)
              ; (debug:print-expression index-node)
              ; (pretty-print proc)
              (if (= (length previous-substitutions) (length children-substitution-list))
                (append previous-substitutions (proc document index-node previous-substitutions))
                previous-substitutions))
            children-substitution-list
            ;all these processor except trivial-process must add its result to current index-node
            ;such as for (app param ...), app's result type could be (return-type (param-type ...))
            ;must extend substitution with (current-index-node-variable = return-type)
            (list 
              ;this should be the first
              trivial-process

              ;their position and order, I don't care.
              define-process
              do-process
              let-process
              if-process
              lambda-process
              record-process

              ;this should be the last
              application-process)))]
      [(and quoted? allow-unquote? (unquote? index-node document))
        ; (pretty-print 'cao8)
        (private-construct-substitution-list 
          document 
          (car children) 
          `(,@base-substitution-list 
            (,variable = ,(index-node-variable (car children))))
          #f 
          #f)]
      [(and quoted? (or (list? expression) (vector? expression)))
        ; (pretty-print 'cao9)
        (let* ([final-result
              (fold-left 
                (lambda (ahead-result current-index-node)
                  (let* ([first (car ahead-result)]
                      [last (cdr ahead-result)]
                      [variable (index-node-variable current-index-node)]
                      [current-children (index-node-children current-index-node)]
                      [children-variables (map index-node-variable current-children)])
                    (if (and (unquote-splicing? current-index-node document) allow-unquote? (not (null? current-children)))
                      `(,(append first children-variables) . 
                        ,(fold-left
                          (lambda (previous-substitutions child-index-node) 
                            (private-construct-substitution-list document child-index-node previous-substitutions #f #f))
                          last
                          current-children))
                      `((,@first ,variable) . 
                        ,(private-construct-substitution-list document current-index-node last allow-unquote? quoted?)))))
                `((,(if (list? expression) 'inner:list? 'inner:vector?)) . ())
                children)]
            [variable-list (car final-result)]
            [extend-substitution-list (cdr final-result)])
          `(,@base-substitution-list ,@extend-substitution-list (,variable = ,variable-list)))]
      [quoted? 
        ; (pretty-print 'cao11)
        (append base-substitution-list 
          (trivial-process document index-node variable expression base-substitution-list allow-unquote? quoted?))]
      [else base-substitution-list])))
)