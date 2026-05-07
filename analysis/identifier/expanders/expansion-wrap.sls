(library (scheme-langserver analysis identifier expanders expansion-wrap)
  (export
    expansion-generator->rule)
  (import 
    (chezscheme)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver util contain))

(define (expansion-generator->rule proc step file-linkage expanded+callee-list memory)
  (lambda (root-file-node root-library-node document index-node)
    (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
        ;taking analysis/identifier/expanders/syntax-rules as an example
        [pairs+expansion (proc root-file-node root-library-node document index-node)])
      (if pairs+expansion
        (let* ([pairs (car pairs+expansion)]
            [expansion-index-node (cdr pairs+expansion)]
            [possible-new-memory `(,expression . ,memory)])
          ; Guard 1: prevent re-expansion of the exact same expression.
          ; Guard 2: cap memory chain length to avoid infinite cascades
          ; (e.g. match -> match-next -> match-one -> ...).
          (if (and (not (contain? memory expression))
                   (< (length memory) 5))
            (step root-file-node root-library-node file-linkage document expansion-index-node expanded+callee-list possible-new-memory))
          (private:shallow-copy pairs expansion-index-node document index-node))
        '()))))

; Shallow-copy is a reference back-propagator:
; it copies identifier-references from the expanded AST back to the
; original macro-call nodes according to the pattern->expansion
; correspondence, so that IDE features (go-to-definition, completion)
; work on user-written code.
(define (private:shallow-copy pairs expansion-index-node document initialization-index-node)
  (let* ([local-identifiers+export-index-node (private:recursive-collect expansion-index-node index-node-references-export-to-other-node)]
      [local-identifiers+import-index-node (private:recursive-collect expansion-index-node index-node-references-import-in-this-node)])
    (for-each 
      (lambda (p)
        (let* ([lis (cdr p)]
            [i (car p)]
            [corresponding-export-ip (assoc i pairs)]

            ; If the expansion-sub-node has no counterpart in pairs (e.g.
            ; ellipsis truncation caused a mismatch), fall back to the
            ; original macro-call node so the reference is not lost.
            [compound-export-list 
              (if corresponding-export-ip 
                (cdr corresponding-export-ip)
                initialization-index-node)]
            [corresponding-import-ip 
              (filter (lambda (entry) (equal? (car entry) i)) local-identifiers+import-index-node)]
            [compound-import-list 
              (if (not (null? corresponding-import-ip))
                (apply append (map cdr corresponding-import-ip))
                '())])
          (for-each
            (lambda (single-compound-export-index-node)
              (for-each
                (lambda (current-identifier)
                  (let* ([ni 
                      (make-identifier-reference 
                        (identifier-reference-identifier current-identifier)
                        document 
                        single-compound-export-index-node
                        initialization-index-node 
                        (identifier-reference-library-identifier current-identifier)
                        (identifier-reference-type current-identifier)
                        '()
                        '()
                        (identifier-reference-top-environment current-identifier))])
                    (index-node-references-export-to-other-node-set!
                      single-compound-export-index-node
                      (append 
                        (index-node-references-export-to-other-node single-compound-export-index-node)
                        `(,ni)))
                    (for-each
                      (lambda (import-index-node)
                        (append-references-into-ordered-references-for document import-index-node `(,ni)))
                      (private:recursive-filter compound-import-list index-node?))))
                lis))
            (cond 
              [(index-node? compound-export-list) `(,compound-export-list)]
              [(list? compound-export-list) (filter index-node? compound-export-list)]
              [else '()]))))
      local-identifiers+export-index-node)))

(define (private:recursive-filter compound-list predicate?)
  (cond 
    [(predicate? compound-list) `(,compound-list)]
    [(null? compound-list) '()]
    [(pair? compound-list) 
      `(,(private:recursive-filter (car compound-list) predicate?) . ,(private:recursive-filter (cdr compound-list) predicate?))]
    [(vector? compound-list)
      (private:recursive-filter (vector->list compound-list) predicate?)]))

(define (private:recursive-collect expansion-index-node proc)
  (let ([current (proc expansion-index-node)]
      [children-results (apply append (map (lambda (child) (private:recursive-collect child proc)) (index-node-children expansion-index-node)))])
    (if (null? current)
      children-results
      `((,expansion-index-node . ,current) . ,children-results))))
) ; end library
