(library (scheme-langserver analysis identifier expanders expansion-wrap)
  (export
    expansion-generator->rule)
  (import 
    (chezscheme)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference))

(define (expansion-generator->rule proc step file-linkage expanded+callee-list possible-new-memory)
  (lambda (root-file-node root-library-node document index-node)
    (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
        ;taking analysis/identifier/expanders/syntax-rules as an example
        [pairs+expansion (proc root-file-node root-library-node document index-node)]
        [pairs (car pairs+expansion)]
        [expansion-index-node (cdr pairs+expansion)])
      (step root-file-node root-library-node file-linkage document expanded+callee-list possible-new-memory)
      (private:shallow-copy pairs expansion-index-node document index-node))))

(define (private:shallow-copy pairs expansion-index-node document initialization-index-node)
  (let* ([local-identifiers+export-index-node (private:recursive-collect expansion-index-node index-node-references-export-to-other-node)]
      [local-identifiers+import-index-node (private:recursive-collect expansion-index-node index-node-references-import-in-this-node)])
    (map 
      (lambda (p)
        (let* ([lis (cdr p)]
            [i (car p)]
            [corresponding-export-ip (assoc i pairs)]

            [compound-export-list 
              (if corresponding-export-ip 
                (cdr corresponding-export-ip)
                #f)]
            [corresponding-import-ip 
              (if compound-export-list
                (filter (lambda (p) (equal? (car p) i))  local-identifiers+import-index-node)
                '())]
            [compound-import-list 
              (if corresponding-import-ip 
                (map cdr corresponding-import-ip)
                '())])
          (map 
            (lambda (single-compound-export-index-node)
              (map 
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
                      compound-export-list
                      (append 
                        (index-node-references-export-to-other-node compound-export-list)
                        `(,ni)))
                    (map 
                      (lambda (import-index-node)
                        (append-references-into-ordered-references-for document import-index-node `(,ni)))
                      (private:recursive-filter compound-import-list index-node?))))
                lis))
            (filter index-node? compound-export-list))))
      local-identifiers+export-index-node)))

(define (private:recursive-filter compound-list predicate?)
  (cond 
    [(predicate? compound-list) `(,compound-list)]
    [(null? compound-list) '()]
    [(pair? compound-list) 
      `(,(private:recursive-filter (car compound-list) predicate?) . ,(private:recursive-filter (cdr compound-list predicate?)))]
    [(vector? compound-list)
      (private:recursive-filter (vector->list compound-list) predicate?)]))

(define (private:recursive-collect expansion-index-node proc)
  (if (null? (proc expansion-index-node))
    '()
    `((,expansion-index-node . ,(proc expansion-index-node)) .
      ,(apply append (map private:recursive-collect (index-node-children expansion-index-node) proc)))))
)