(library (scheme-langserver analysis identifier rules let)
  (export library-define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (let-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (try
      (match expression
        [('let* ((identifier no-use ... ) **1 ) _ ... ) 
          (guard-for 'let* '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([exclude '()] 
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (loop (append exclude (process (car rest) index-node exclude document)) (cdr rest))))]
        [('let ((identifier no-use ... ) **1 ) _ ... ) 
          (guard-for 'let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (begin
                (index-node-excluded-references-set! 
                  (cadr (index-node-children index-node))
                  (append 
                    (index-node-excluded-references (cadr (index-node-children index-node)))
                    (process (car rest) index-node '() document)))
                (loop '() (cdr rest)))))]
        [('letrec* ((identifier no-use ... ) **1 ) _ ... ) 
          (guard-for 'let* '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([exclude '()] 
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (loop (append exclude (process (car rest) index-node exclude document)) (cdr rest))))]
        [('letrec ((identifier no-use ... ) **1 ) _ ... ) 
          (guard-for 'letrec '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (begin
                (index-node-excluded-references-set! 
                  (cadr (index-node-children index-node))
                  (append 
                    (index-node-excluded-references (cadr (index-node-children index-node)))
                    (process (car rest) index-node '() document)))
                (loop '() (cdr rest)))))]
        [else '()])
      (except 
        [else '()]))))

(define (process index-node let-node exclude document )
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [reference 
          (make-identifier-reference
            (string->symbol expression)
          document
          index-node
          '())])
    (index-node-references-export-to-other-node-set! 
      index-node
      (append 
        (index-node-references-export-to-other-node index-node)
          `(,reference)))

    (index-node-references-import-in-this-node-set! 
      let-node
      (append 
        (index-node-references-import-in-this-node let-node)
          `(,reference)))

    (index-node-excluded-references-set! 
      (index-node-parent index-node)
      (append 
        (index-node-excluded-references index-node)
        exclude))

    `(,reference)))
)