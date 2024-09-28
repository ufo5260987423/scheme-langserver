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
    (scheme-langserver analysis identifier macro-expander)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (self-defined-syntax-process identifier-reference callee-index-node callee-document stepper)
  (let* ([expanded-expression (expand:step-by-step identifier-reference callee-index-node callee-document)]
      [virtual-index-node-list 
        (private:init-virtual-index-node-list expanded-expression (index-node-parent (identifier-reference-initialization-index-node identifier-reference)) callee-document)]
      [stepped-virtual-index-node-list (map stepper virtual-index-node-list)]
      [callee-expression (annotation-stripped (index-node-datum/annotations callee-index-node))]
      [callee-symbol-index-node-list (private:get-symbol-index-node-children callee-index-node)]
      [virtual-symbol-index-node-list (apply append (map private:get-symbol-index-node-children stepped-virtual-index-node-list))])
    ;; (map 
    ;;   (lambda (callee-symbol-index-node)
    ;;     (map 
    ;;       (lambda (virtual-symbol-index-node)
    ;;         (map 
    ;;           (lambda (identifier-reference)
    ;;             )
    ;;           (index-node-references-export-to-other-node virtual-symbol-index-node)))
    ;;       (filter 
    ;;         (lambda (virtual-symbol-index-node) 
    ;;           (equal? 
    ;;             (annotation-stripped (index-node-datum/annotations callee-symbol-index-node))
    ;;             (annotation-stripped (index-node-datum/annotations virtual-symbol-index-node)))) 
    ;;           virtual-symbol-index-node-list)))
    ;;   callee-symbol-index-node-list)
    '()
      ))

(define (private:process-identifier-claiment callee-index-node identifier-reference virtual-symbol-index-node-list)
  '()
)

(define (private:init-virtual-index-node-list expanded-expression parent-index-node document)
  (map 
    (lambda (item) (init-index-node parent-index-node item))
    (source-file->annotations (with-output-to-string (lambda () (pretty-print expanded-expression))) (uri->path (document-uri document)))))

(define (private:get-symbol-index-node-children target-index-node)
  (let ([expression (annotation-stripped (index-node-datum/annotations target-index-node))]
      [children (index-node-children target-index-node)])
    (cond 
      [(not (null? children)) (apply append (map private:get-symbol-index-node-children children))]
      [(symbol? expression) target-index-node]
      [else '()])))
)