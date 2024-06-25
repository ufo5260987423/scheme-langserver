(library (scheme-langserver analysis identifier rules self-defined-syntax)
  (export 
    self-defined-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util path)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis local-expand)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (self-defined-syntax-process root-file-node root-library-node document index-node file-linkage stepper)
  (let* ([candidates (private-get-candidates index-node)]
      [origin-annotation-list (private-expand root-library-node document index-node file-linkage (annotation-stripped (index-node-datum/annotations index-node)) stepper)]
      [candidate->expanded-index-node-list
        (fold-left 
          (lambda (left candidate)
            (let* ([differences 
                  (pick-differences 
                    origin-annotation-list 
                    (private-construct-different-annotation-list root-library-node file-linkage document index-node candidate stepper))]
                [heads (filter is-leaf? (filter index-node? (map car differences)))])
              (append left `((,candidate . ,heads)))))
          '()
          candidates)]
      [identifier-claiment-pair 
        (filter 
          (lambda (index-node-list) (not (null? (apply append (map index-node-references-export-to-other-node index-node-list))))) 
          (map cdr candidate->expanded-index-node-list))]
      [to-ground-pair 
        (filter 
          (lambda (index-node-list) (null? (apply append (map index-node-references-export-to-other-node index-node-list))))
          (map cdr candidate->expanded-index-node-list))]
      [to-ground-candidate->old-identifier-references 
        (map 
          (lambda (p) 
            `(,(car p) . ,(dedupe (apply append (map (lambda (i) (find-available-references-for document i)) (cdr p))))))
          to-ground-pair)]
      [identifier-reference:old->new
        (map 
          (lambda (p)
            (let* ([head (car p)]
                [tail (cdr p)]
                [old-identifier-references (apply append (map index-node-references-export-to-other-node tail))]
                [old->new
                  (map 
                    (lambda (old)
                      `(,old . 
                        ,(make-identifier-reference
                          (unwrap-symbol (identifier-reference-identifier old))
                          document
                          head
                          index-node
                          '()
                          (identifier-reference-type old)
                          '()
                          '())))
                    old-identifier-references)])
              (index-node-references-export-to-other-node-set! head 
                (append 
                  (index-node-references-export-to-other-node head)
                  (map cdr old->new)))
              old->new))
          identifier-claiment-pair)]
          )
  (map
    (lambda (p)
      (let* ([old (car p)]
          [new (cdr p)]
          [l (filter (lambda (t-p) (contain? (cdr t-p) old)) to-ground-candidate->old-identifier-references)]
          [nearest-common-ancestor (private-pick-nearest-common-ancestor (map (lambda (l0) (reverse (private-pick-ancestor (car l0) index-node))) l))])
        (if (not (null? nearest-common-ancestor))
          (append-references-into-ordered-references-for document nearest-common-ancestor `(,new)))))
    identifier-reference:old->new)))

(define (private-pick-nearest-common-ancestor target-list)
  (if (contain? target-list '())
    '()
    (if (apply private-all-equal? (map car target-list))
      (let ([tmp (private-pick-nearest-common-ancestor (map cdr target-list))])
        (if (null? tmp)
          (car (map car target-list))
          tmp))
      '())))

(define (private-all-equal? head . tail)
  (if (null? tail)
    #t
    (and (equal? head (car tail)) (apply private-all-equal? tail))))

(define (private-pick-ancestor from to)
  (if (or (null? from) (equal? from to))
    `(,from)
    `(,from . ,(private-pick-ancestor (index-node-parent from) to))))

(define (private-construct-different-annotation-list root-library-node file-linkage document origin-index-node current-index-node stepper)
  (let* ([origin-start (index-node-start origin-index-node)]
      [origin-end (index-node-start origin-index-node)]
      [text (document-text document)]
      [current-start (index-node-start current-index-node)]
      [current-end (index-node-end current-index-node)]
      [current-text (substring text current-start current-end)]
      [pre-text (substring text origin-start current-start)]
      [tail-text (substring text current-end origin-end)]
      [new-text (string-append pre-text "prefix-" current-text tail-text)]
      [port (open-string-input-port new-text)]
      [to-eval (read port)])
    (private-expand root-library-node document origin-index-node to-eval file-linkage stepper)))

(define (pick-differences index-nodes0 index-nodes1)
  (cond 
    [(and (null? index-nodes0) (null? index-nodes1)) '()]
    [(null? index-nodes0) `((() . ,index-nodes1))]
    [(null? index-nodes1) `((,index-nodes0))]
    [else 
      (let* ([index-node0 (car index-nodes0)]
          [index-node1 (car index-nodes1)]
          [structure0 (annotation-stripped (index-node-datum/annotations index-node0))]
          [structure1 (annotation-stripped (index-node-datum/annotations index-node1))]
          [children0 (index-node-children index-node0)]
          [children1 (index-node-children index-node1)])
        (cond 
          [(unwrap-equal? structure0 structure1) (pick-differences (cdr index-nodes0) (cdr index-nodes1))]
          [(and (vector? structure0) (vector? structure1)) (pick-differences children0 children1)]
          [(and (pair? structure0) (pair? structure1)) (pick-differences children0 children1)]
          [else `((,index-node0 . ,index-node1) . ,(pick-differences (cdr index-nodes0) (cdr index-nodes1)))]))]))

(define (private-expand root-library-node document index-node to-eval file-linkage stepper)
  (try
    (let* ([tmp-result (local-expand to-eval document root-library-node file-linkage)]
        [annotation-list (map (lambda (e) (source-file->annotations e (uri->path (document-uri document)))) tmp-result)])
      (map (lambda (a) (index-node-parent-set! a index-node)) annotation-list)
      (map stepper annotation-list)
      annotation-list)
    (except c [else '()])))

(define (private-get-candidates index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (cond 
      [(equal? 'quote expression) '()]
      [(equal? 'quasiquote expression) '()]
      [(equal? 'unquote expression) '()]
      [(equal? 'unquote-splicing expression) '()]
      [(equal? 'unsyntax-splicing expression) '()]
      [(equal? 'unsyntax expression) '()]
      [(equal? 'syntax expression) '()]
      [(equal? 'quasisyntax expression) '()]

      [(symbol? expression) `(,index-node)]
      [(not (null? (index-node-children index-node))) (apply append (map private-get-candidates (index-node-children index-node)))]
      [else '()])))

(define (private-get-all-identifiers index-node method)
  (apply append (method index-node) (map (lambda (i) (private-get-all-identifiers i method)) (index-node-children index-node))))
)
