(library (scheme-langserver analysis identifier expanders expansion-wrap)
  (export
    expansion-generator->rule)
  (import 
    (chezscheme)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver util contain))


(define (expansion-generator->rule proc step file-linkage expanded+callee-list memory . maybe-expander-ref)
  (let ([expander-ref (if (null? maybe-expander-ref) #f (car maybe-expander-ref))])
    (lambda (root-file-node root-library-node document index-node)
      (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
          [pairs+expansion (proc root-file-node root-library-node document index-node)])
        (if pairs+expansion
          (let* ([pairs (car pairs+expansion)]
              [expansion-index-node (cdr pairs+expansion)]
              [possible-new-memory `(,expression . ,memory)]
              [expander-doc (if expander-ref (identifier-reference-document expander-ref) #f)]
              [previous-all-pairs (if (null? expanded+callee-list)
                                    '()
                                    (list-ref (car expanded+callee-list) 4))]
              [new-all-pairs (append pairs previous-all-pairs)]
              [new-expanded+callee-list 
                (cons `(,expansion-index-node ,index-node ,expander-doc ,pairs ,new-all-pairs) expanded+callee-list)])
            ; Guard 1: prevent re-expansion of the exact same expression.
            ; Guard 2: cap memory chain length to avoid infinite cascades
            ; (e.g. match -> match-next -> match-one -> ...).
            (if (and (not (contain? memory expression)) (< (length memory) 15))
              (step root-file-node root-library-node file-linkage document expansion-index-node new-expanded+callee-list possible-new-memory))
            (private:shallow-copy pairs (extract-all-pairs new-expanded+callee-list) expansion-index-node document index-node))
          '())))))

(define (private:recursive-filter compound-list predicate?)
  (let loop ([stack (list compound-list)] [acc '()])
    (if (null? stack)
      (reverse acc)
      (let ([current (car stack)] [rest (cdr stack)])
        (cond 
          [(predicate? current) 
            (loop rest (cons current acc))]
          [(null? current) 
            (loop rest acc)]
          [(pair? current) 
            (loop (cons (car current) (cons (cdr current) rest)) acc)]
          [(vector? current)
            (loop (cons (vector->list current) rest) acc)]
          [else 
            (loop rest acc)])))))

(define (private:recursive-collect expansion-index-node proc)
  (let loop ([stack (list expansion-index-node)] [acc '()])
    (if (null? stack)
      (reverse acc)
      (let* ([node (car stack)]
          [current (proc node)]
          [new-acc (if (null? current) acc (cons `(,node . ,current) acc))]
          [children (index-node-children node)]
          [new-stack 
            (let child-loop ([cs children] [s (cdr stack)])
              (if (null? cs)
                s
                (child-loop (cdr cs) (cons (car cs) s))))])
        (loop new-stack new-acc)))))

(define (private:find-nodes-by-symbol node sym)
  (if (index-node? node)
    (let loop ([stack (list node)] [acc '()])
      (if (null? stack)
        (reverse acc)
        (let* ([current (car stack)]
            [new-acc (if (eq? sym (annotation-stripped (index-node-datum/annotations current)))
                      (cons current acc)
                      acc)]
            [children (index-node-children current)]
            [new-stack 
              (let child-loop ([cs children] [s (cdr stack)])
                (if (null? cs)
                  s
                  (child-loop (cdr cs) (cons (car cs) s))))])
          (loop new-stack new-acc))))
    '()))

; Shallow-copy is a reference back-propagator:
; it copies identifier-references from the expanded AST back to the
; original macro-call nodes according to the pattern->expansion
; correspondence, so that IDE features (go-to-definition, completion)
; work on user-written code.
(define (extract-all-pairs expanded+callee-list)
  (if (null? expanded+callee-list)
    '()
    (list-ref (car expanded+callee-list) 4)))

(define (build-reverse-map all-pairs)
  (let ([ht (make-eq-hashtable)])
    (for-each
      (lambda (pair)
        (let ([key (cdr pair)])
          (when (and (index-node? key) (not (hashtable-contains? ht key)))
            (hashtable-set! ht key (car pair)))))
      all-pairs)
    ht))

(define (private:sync-to-parent-expansion target-node export-list reverse-map document initialization-index-node)
  (let ([parent-node (hashtable-ref reverse-map target-node #f)])
    (when parent-node
      (for-each
        (lambda (current-identifier)
          (let ([ni 
              (make-identifier-reference 
                (identifier-reference-identifier current-identifier)
                document 
                parent-node
                initialization-index-node 
                (identifier-reference-library-identifier current-identifier)
                (identifier-reference-type current-identifier)
                '()
                '()
                (identifier-reference-top-environment current-identifier))])
            (identifier-reference-speculative?-set! ni #t)
            (index-node-references-export-to-other-node-set!
              parent-node
              (append 
                (index-node-references-export-to-other-node parent-node)
                `(,ni)))))
        export-list))))

(define (private:shallow-copy pairs all-pairs expansion-index-node document initialization-index-node)
  (let* ([reverse-map (build-reverse-map all-pairs)]
      [local-identifiers+export-index-node (private:recursive-collect expansion-index-node index-node-references-export-to-other-node)]
      [local-identifiers+import-index-node (private:recursive-collect expansion-index-node index-node-references-import-in-this-node)]
      ;Inserting each new reference one-by-one into the ordered lists costs
      ;one full merge pass per reference (quadratic in the number of
      ;back-propagated references). Queue them per target instead and flush
      ;once per target after the walk.
      [pending-inserts-ht (make-eq-hashtable)])
    (define (queue-insert! import-index-node ni)
      (hashtable-update! pending-inserts-ht
        import-index-node
        (lambda (old) (cons ni old))
        '()))
    (for-each 
      (lambda (p)
        (let* ([lis (cdr p)]
            [i (car p)]
            [corresponding-export-ip (assoc i pairs)]

            ; If the expansion-sub-node has no counterpart in pairs (e.g.
            ; ellipsis truncation caused a mismatch), fall back to the
            ; original macro-call node so the reference is not lost.
            [compound-export-list 
              (cond
                [corresponding-export-ip 
                  (cdr corresponding-export-ip)]
                ; PLAN Step 3: composite node not in local-pairs, try all-pairs
                [(assoc i all-pairs) => cdr]
                [else 
                  (let ([sym (annotation-stripped (index-node-datum/annotations i))])
                    (if (symbol? sym)
                      (let ([matches (private:find-nodes-by-symbol initialization-index-node sym)])
                        (if (null? matches)
                          initialization-index-node
                          matches))
                      initialization-index-node))])]
)          (for-each
            (lambda (single-compound-export-index-node)
              ; Primary attachment: write to callee tree (existing behavior)
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
                    (identifier-reference-speculative?-set! ni #t)
                    (let ([current-exports (index-node-references-export-to-other-node single-compound-export-index-node)])
                      (if (not (find (lambda (e) (eq? (identifier-reference-identifier current-identifier) (identifier-reference-identifier e))) current-exports))
                        (begin
                          (index-node-references-export-to-other-node-set!
                            single-compound-export-index-node
                            (append current-exports `(,ni)))
)
                        '()))))
                lis)
              ; Reverse-map sync: also write to parent expansion tree so outer layer can see it
              (private:sync-to-parent-expansion single-compound-export-index-node lis reverse-map document initialization-index-node))
            (cond 
              [(index-node? compound-export-list) `(,compound-export-list)]
              [(list? compound-export-list) (filter index-node? compound-export-list)]
              [else '()]))))
      local-identifiers+export-index-node)
    ;Import side, independent of the export walk: for every
    ;import-bearing skeleton node, copy its references to the compound
    ;counterpart (call-site) node so use-site resolution works. (The old
    ;code cross-joined import entries against EXPORT-bearing nodes and
    ;filtered references by index-node?, leaving this side permanently
    ;unwired.)
    (for-each
      (lambda (import-entry)
        ;counterpart: the compound pair of the import-bearing skeleton
        ;node. Root and symbol-leaf shapes have no node-level pair (the
        ;pairs walk only pairs index-node compounds and child zips), so
        ;fall back to the macro call node -- the import's scope is the
        ;whole expansion, which at the call site is exactly the call.
        (let ([counterpart
            (let ([p (assoc (car import-entry) pairs)])
              (if p
                (let ([c (cdr p)])
                  (if (index-node? c) c initialization-index-node))
                initialization-index-node))])
          (when (index-node? counterpart)
            (for-each
              (lambda (import-reference)
                (let ([ni
                    (make-identifier-reference
                      (identifier-reference-identifier import-reference)
                      document
                      counterpart
                      initialization-index-node
                      (identifier-reference-library-identifier import-reference)
                      (identifier-reference-type import-reference)
                      '()
                      '()
                      (identifier-reference-top-environment import-reference))])
                  (identifier-reference-speculative?-set! ni #t)
                  (queue-insert! counterpart ni)))
              (cdr import-entry)))))
      local-identifiers+import-index-node)
    (when (positive? (hashtable-size pending-inserts-ht))
      (vector-for-each
        (lambda (import-index-node)
          (append-references-into-ordered-references-for document import-index-node
            (hashtable-ref pending-inserts-ht import-index-node '())))
        (hashtable-keys pending-inserts-ht)))))
) ; end library
