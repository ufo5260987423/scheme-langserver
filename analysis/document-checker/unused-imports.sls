(library (scheme-langserver analysis document-checker unused-imports)
  (export 
    check-unused-imports)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis identifier util))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unused import detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:collect-import-usages document)
  (let ([used-ht (make-eq-hashtable)]
      [import-clauses '()]
      [duplicate-seen (make-hashtable equal-hash equal?)]
      [duplicate-seen-spans (make-hashtable equal-hash equal?)])
    (let loop ([nodes (document-index-node-list document)] [in-import? #f])
      (for-each
        (lambda (node)
          (let ([expression (annotation-stripped (index-node-datum/annotations node))])
            (cond
              [(and (not (index-node-shared-reference node)) (pair? expression) (eq? 'import (car expression)))
                (for-each 
                  (lambda (child) (private:check-duplicate-import-clause document child duplicate-seen duplicate-seen-spans))
                  (cdr (index-node-children node)))
                (set! import-clauses 
                  (append (cdr (index-node-children node)) import-clauses))
                (loop (index-node-children node) #t)]
              [in-import?
                (loop (index-node-children node) #t)]
              [else
                (if (and (null? (index-node-children node)) (symbol? expression))
                  (for-each
                    (lambda (ref)
                      (if (not (eq? (identifier-reference-document ref) document))
                        (eq-hashtable-set! used-ht ref #t)))
                    (find-available-references-for document node expression)))
                (loop (index-node-children node) #f)])))
        nodes))
    (values used-ht (reverse import-clauses))))

(define (check-unused-imports document)
  (let-values ([(used-ht import-clauses) (private:collect-import-usages document)])
    (let ([seen (make-eq-hashtable)])
      (for-each 
        (lambda (clause-node) (private:check-import-clause document clause-node used-ht seen))
        import-clauses))))

(define (private:check-import-clause document index-node used-ht seen)
  (let ([expression (annotation-stripped (index-node-datum/annotations index-node))])
    (match expression
      [('only (library-identifier **1) (? symbol? identifier) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [idents identifier])
          (if (not (null? nodes))
            (let* ([current-node (car nodes)]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car idents) seen))
              (loop (cdr nodes) (cdr idents)))))]
      [('except (library-identifier **1) (? symbol? identifier) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [idents identifier])
          (if (not (null? nodes))
            (let* ([current-node (car nodes)]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car idents) seen))
              (loop (cdr nodes) (cdr idents)))))]
      [('rename (library-identifier **1) ((? symbol? external-name) (? symbol? internal-name)) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [internal-names internal-name])
          (if (not (null? nodes))
            (let* ([current-node (cadr (index-node-children (car nodes)))]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car internal-names) seen))
              (loop (cdr nodes) (cdr internal-names)))))]
      [('alias (library-identifier **1) ((? symbol? external-name) (? symbol? internal-name)) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [internal-names internal-name])
          (if (not (null? nodes))
            (let* ([current-node (cadr (index-node-children (car nodes)))]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car internal-names) seen))
              (loop (cdr nodes) (cdr internal-names)))))]
      [('prefix (library-identifier **1) (? symbol? prefix-id))
        '()]
      [('for :_ ...)
        '()]
      [(library-identifier **1)
        ; Plain imports now also attach their references to the library-identifier
        ; node itself (see library-import.sls), so checking this node tells us
        ; whether any binding introduced by this specific import was used.
        (let ([refs (index-node-references-import-in-this-node index-node)])
          (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
            (private:append-unused-import-diagnose document index-node (library-identifier->string expression) seen)
            '()))]
      [else '()])))

(define (private:append-unused-import-diagnose document index-node identifier seen)
  (if (not (eq-hashtable-contains? seen index-node))
    (begin
      (eq-hashtable-set! seen index-node #t)
      (append-new-diagnoses document
        `(,(index-node-start index-node) ,(index-node-end index-node) 2
          ,(string-append "Unused import: " (if (symbol? identifier) (symbol->string identifier) identifier))
          "import" "unused-import")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicate import detection (merged into collect-import-usages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:check-duplicate-import-clause document index-node seen seen-spans)
  ; Tolerant re-parsing of a file with syntax errors can leave duplicated
  ; subtrees in the index tree, so the same clause may be visited twice and
  ; be misreported as a duplicate import.  Skip nodes whose source span has
  ; already been checked; genuinely duplicated clauses have distinct spans.
  (let ([span (cons (index-node-start index-node) (index-node-end index-node))])
    (if (hashtable-contains? seen-spans span)
      '()
      (begin
        (hashtable-set! seen-spans span #t)
        (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
            [library-identifier (resolve-import-library-identifier expression)])
          (when (and (pair? library-identifier) (not (null? library-identifier)))
            (if (hashtable-contains? seen library-identifier)
              (private:append-duplicate-import-diagnose document index-node library-identifier)
              (hashtable-set! seen library-identifier #t)))))))) 

(define (private:append-duplicate-import-diagnose document index-node library-identifier)
  (append-new-diagnoses document
    `(,(index-node-start index-node) ,(index-node-end index-node) 2
      ,(string-append "Duplicate import: " (library-identifier->string library-identifier))
      "import" "duplicate-import")))
)
