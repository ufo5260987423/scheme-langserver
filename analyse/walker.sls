(library (scheme-langserver analyse walker)
  (export walk)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver analyse document)
    (scheme-langserver analyse index)
    (scheme-langserver analyse virtual-file-system))

(define (walk . args)
  (match args
    ([(? string? to-path) (? file-node? from-node)]
      (let ([current-path (file-node-path from-node)])
        (if (string-prefix? current-path to-path)
          (if (equal? to-path current-path)
            from-node
            (walk to-path 
              (find (lambda (child) (string-suffix? (file-node-path child) to-path)) 
                (file-node-children from-node)))))))
    (else #f)
  ))
)