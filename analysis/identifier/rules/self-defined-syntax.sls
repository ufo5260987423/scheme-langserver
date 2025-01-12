(library (scheme-langserver analysis identifier rules self-defined-syntax)
  (export 
    self-defined-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
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

(define (self-defined-syntax-process top-identifier-reference callee-index-node callee-document old-expanded+callee-list stepper)
  (let ([template+callees (generate-pair:template+callee top-identifier-reference callee-index-node callee-document)]
      [expanded-expression-list (expand:step-by-step top-identifier-reference callee-index-node callee-document)])
    (map 
      (lambda (expanded-expression)
    ; (pretty-print 'aa2)
    ; (pretty-print (document-uri callee-document))
    ; (pretty-print expanded-expression)
        (let ([pre 
                (source-file->annotations 
                  (with-output-to-string (lambda () (pretty-print expanded-expression)))
                  (uri->path (document-uri (identifier-reference-document top-identifier-reference))))])
          (if (pair? pre)
            (let* ([expanded-index-node 
                  (init-index-node 
                    (identifier-reference-initialization-index-node top-identifier-reference) 
                    (car pre))]
                [callee+expanded-pairs (generate-pair:callee+expanded template+callees expanded-index-node top-identifier-reference callee-index-node callee-document)]
                [expanded+callee-list (private:reverse-pair callee+expanded-pairs)])
              (stepper (identifier-reference-document top-identifier-reference) expanded-index-node (append expanded+callee-list old-expanded+callee-list))))))
      expanded-expression-list)))

(define (private:reverse-pair callee+expanded-pairs)
  (apply append 
    (map 
      (lambda (pair)
        (map 
          (lambda (i) `(,i . ,(car pair)))
          (cdr pair)))
      callee+expanded-pairs)))
)