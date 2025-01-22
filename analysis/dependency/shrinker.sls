(library (scheme-langserver analysis dependency shrinker)
  (export shrink-paths)
  (import 
    (chezscheme)
    (scheme-langserver analysis dependency file-linkage))

(define (shrink-paths linkage paths)
  (let* ([path->id-map (file-linkage-path->id-map linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [ids (map (lambda (current-path) (hashtable-ref path->id-map current-path #f)) paths)]
      [shrinked-ids (shrink-ids linkage ids '())])
    (map 
      (lambda (ids) 
        (map (lambda (id) (hashtable-ref id->path-map id #f)) ids))
      shrinked-ids)))

(define (shrink-ids linkage ids result)
  (let ([matrix (file-linkage-matrix linkage)]
      [id->path-map (file-linkage-id->path-map linkage)])
    (if (null? ids)
      result
      (let* ([current-from (car ids)]
          [rest-from (cdr ids)]
          [current-from-path (hashtable-ref id->path-map current-from #f)])
        (if (null? result)
          (shrink-ids linkage rest-from `((,current-from)))
          (if (zero? 
              (apply + 
                (map 
                  (lambda (to) 
                    (file-linkage-take 
                      linkage 
                      current-from-path 
                      (hashtable-ref id->path-map to 0))) 
                  (car (reverse result)))))
            (shrink-ids 
              linkage
              rest-from 
              (append 
                (reverse (cdr (reverse result))) 
                (list (append (car (reverse result)) `(,current-from)))))
            (shrink-ids 
              linkage
              rest-from 
              (append 
                result
                (list `(,current-from))))))))))
)