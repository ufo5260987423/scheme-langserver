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

(define (self-defined-syntax-process identifier-reference callee-index-node callee-document old-expanded-index-node stepper)
  (map 
    (lambda (expanded-expression)
      (let* ([identifier-reference (root-ancestor identifier-reference)]
          [expanded-index-node 
            (init-index-node 
              (identifier-reference-initialization-index-node identifier-reference) 
              (car 
                (source-file->annotations 
                  (with-output-to-string (lambda () (pretty-print expanded-expression)))
                  (uri->path (document-uri (identifier-reference-document identifier-reference))))))]
          [callee+expanded-pairs (generate-pair:callee+expanded `(,expanded-index-node) identifier-reference callee-index-node callee-document)]
          [expanded+callee-list (private:reverse-pair callee+expanded-pairs)])
        (stepper (identifier-reference-document identifier-reference) expanded-index-node (append expanded+callee-list old-expanded-index-node))
        '()
      ))
    (expand:step-by-step identifier-reference callee-index-node callee-document)))

(define (private:reverse-pair callee+expanded-pairs)
  (apply append 
    (map 
      (lambda (pair)
        (map 
          (lambda (i) `(,i . ,(car i)))
          (cdr pair)))
      callee+expanded-pairs)))

(define (private:process-identifier-claiment callee-index-node identifier-reference virtual-symbol-index-node-list)
  '()
)

(define (private:get-symbol-index-node-children target-index-node)
  (let ([expression (annotation-stripped (index-node-datum/annotations target-index-node))]
      [children (index-node-children target-index-node)])
    (cond 
      [(not (null? children)) (apply append (map private:get-symbol-index-node-children children))]
      [(symbol? expression) target-index-node]
      [else '()])))
)