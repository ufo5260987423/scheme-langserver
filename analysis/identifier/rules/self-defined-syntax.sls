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
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (self-defined-syntax-process identifier-reference calling-available-references stepper)
  (let* ([initialization-index-node (identifier-reference-initialization-index-node identifier-reference)]
      [init-expression (annotation-stripped (index-node-datum/annotations initialization-index-node))])
    (match init-expression
      [(head fuzzy **1) 
        '()
        ;; (map 
        ;;   (lambda (current-identifier-reference)
        ;;     (let ([i (identifier-reference-identifier current-identifier-reference)]
        ;;         [l (identifier-reference-library-identifier current-identifier-reference)])
        ;;       (cond 
        ;;         [(and 
        ;;           (find meta-library? is)
        ;;           (or 
        ;;             (equal? i 'with-syntax)
        ;;             (equal? i 'define-syntax)
        ;;             (equal? i 'let-syntax)))
        ;;           (private:dispatch i identifier-reference calling-available-references stepper)]
        ;;         [else 
        ;;           ;todo
        ;;           '()
        ;;         ])))
        ;;   (dedupe (map root-ancestor (find-available-references-for document initialization-index-node head))))
          ]
      [else '()])))

;; (define (private:dispatch how-to-define-syntax-variable identifier-reference calling-available-references stepper)
;;   (let* ([index-node (identifier-reference-index-node identifier-reference)]
;;       [parent-index-node (index-node-parent index-node)]
;;       [expression (annotation-stripped (index-node-datum/annotations parent-index-node))]
;;       [target 
;;         (match expression 
;;           [(head mid tail ...)
;;             (case how-to-define-syntax-variable
;;               ['define-syntax 
;;                 (if (null? tail)
;;                   '()
;;                   (car tail))]
;;               [else mid])]
;;           [else '()])])
;;     (if (null? target) 
;;       '()
;;       (begin 
;;       ))
;;     (cond 
;;       [(null? target) '()]
;;       [(equal? how-to-define-syntax-variable 'define-syntax) ()]
;;       [(equal? how-to-define-syntax-variable 'let-syntax) ]
;;       ))
;;       )
)
