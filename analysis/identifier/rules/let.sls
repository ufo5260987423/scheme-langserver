(library (scheme-langserver analysis identifier rules let)
  (export let-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; procedure parameter variable syntax-variable let-loop
(define (let-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [('let (? symbol? loop-identifier) (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let ([loop-reference-list (private-process index-node (cadr (index-node-children index-node)) index-node '() document 'let-loop)]);here, SCIP says the 'let-loop is a 'procedure, but aparrently, a macro expander is needed here.
            (let loop ([rest (index-node-children (caddr (index-node-children index-node)))])
              (if (not (null? rest))
                (let* ([identifier-parent-index-node (car rest)]
                      [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                  (index-node-excluded-references-set! 
                    identifier-parent-index-node
                    (append 
                      (index-node-excluded-references identifier-parent-index-node)
                      (private-process index-node identifier-index-node index-node loop-reference-list document 'variable)))
                  (loop (cdr rest))))))]
        [('let (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (private-process index-node identifier-index-node index-node '() document 'variable)))
                (loop (cdr rest)))))]
        [('fluid-let (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'fluid-let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (private-process index-node identifier-index-node index-node '() document 'variable)))
                (loop (cdr rest)))))]
        [('fluid-let-syntax (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'fluid-let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (private-process index-node identifier-index-node index-node '() document 'syntax-variable)))
                (loop (cdr rest)))))]
        [('let-syntax (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'let-syntax '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (private-process index-node identifier-index-node index-node '() document 'syntax-variable)))
                (loop (cdr rest)))))]
        [('let-values (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'let-values '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (private-process index-node identifier-index-node index-node '() document 'syntax-variable)))
                (loop (cdr rest)))))]
        [('let* (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'let* '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([include '()] 
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))]
                    [reference-list (private-process index-node identifier-index-node index-node '() document 'variable)])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    reference-list))
                (index-node-references-import-in-this-node-set! 
                  identifier-parent-index-node
                  (sort-identifier-references
                    (append 
                      (index-node-references-import-in-this-node identifier-parent-index-node)
                      include)))
                (loop (append include reference-list) (cdr rest)))))]
        [('let*-values (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'let*-values '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([include '()] 
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))]
                    [reference-list (private-process index-node identifier-index-node index-node '() document 'variable)])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    reference-list))
                (index-node-references-import-in-this-node-set! 
                  identifier-parent-index-node
                  (sort-identifier-references
                    (append 
                      (index-node-references-import-in-this-node identifier-parent-index-node)
                      include)))
                (loop (append include reference-list) (cdr rest)))))]
        [('letrec (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'letrec '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([exclude '()]
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (loop (append exclude (private-process index-node identifier-index-node index-node exclude document 'variable)) (cdr rest)))))]
        [('letrec-syntax (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'letrec-syntax '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([exclude '()]
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (loop (append exclude (private-process index-node identifier-index-node index-node exclude document 'syntax-variable)) (cdr rest)))))]
        [('letrec* (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
          (guard-for document index-node 'letrec* '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([include '()] 
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))]
                    [reference-list (private-process index-node identifier-index-node index-node '() document 'variable)])
                (index-node-references-import-in-this-node-set! 
                  identifier-parent-index-node
                  (sort-identifier-references
                    (append 
                      (index-node-references-import-in-this-node identifier-parent-index-node)
                      include)))
                (loop (append include reference-list) (cdr rest)))))]
        [else '()])
      (except c
        [else '()]))))

(define (private-process initialization-index-node index-node let-node exclude document type)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [reference 
        (make-identifier-reference
          expression
          document
          index-node
          initialization-index-node
          '()
          type
          '()
          '())])
    (index-node-references-export-to-other-node-set! 
      index-node
      (append 
        (index-node-references-export-to-other-node index-node)
          `(,reference)))

    (index-node-references-import-in-this-node-set! 
      let-node
      (sort-identifier-references
        (append 
          (index-node-references-import-in-this-node let-node)
            `(,reference))))

    (index-node-excluded-references-set! 
      (index-node-parent index-node)
      (append 
        (index-node-excluded-references index-node)
        exclude))

    `(,reference)))
)
