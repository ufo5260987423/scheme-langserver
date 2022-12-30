(library (scheme-langserver analysis dependency shrinker)
  (export shrink-paths)
  (import 
    (chezscheme)
    (scheme-langserver analysis dependency file-linkage))

(define (shrink-paths linkage paths)
  (let* ([path->id-map (file-linkage-path->id-map linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [matrix (file-linkage-matrix linkage)]
      [ids (map (lambda (current-path) (hashtable-ref path->id-map current-path #f)) paths)]
      [shrinked-ids (shrink-ids matrix ids '())])
    (map 
      (lambda (ids) 
        (map (lambda (id) (hashtable-ref id->path-map id #f)) ids))
      shrinked-ids)))

(define (shrink-ids matrix ids result)
      (pretty-print 'ok0)
  (if (null? ids)
    result
    (let ([current-from (car ids)]
        [rest-from (cdr ids)])
      (pretty-print 'ok1)
      (pretty-print (null? result))
      (if (null? result)
        (shrink-ids matrix rest-from (append result (list `(current-from))))
        (if (zero? 
            (apply + 
              (map (lambda (to) (file-linkage-take matrix current-from to)) (car (reverse result)))))
          (shrink-ids 
            matrix 
            rest-from 
            (append 
              (reverse (cdr (reverse result))) 
              (list (append result current-from))))
          (shrink-ids 
            matrix 
            rest-from 
            (append 
              result
              (list `(,current-from)))))))))
)