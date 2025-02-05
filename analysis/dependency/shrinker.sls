(library (scheme-langserver analysis dependency shrinker)
  (export shrink-paths)
  (import 
    (chezscheme)
    (scheme-langserver analysis dependency file-linkage)
    (scheme-langserver util matrix))

(define (shrink-paths linkage paths)
  (let* ([path->id-map (file-linkage-path->id-map linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [ids (map (lambda (current-path) (hashtable-ref path->id-map current-path #f)) paths)]
      [shrinked-ids (shrink-ids (file-linkage-matrix linkage) ids '())])
    (map 
      (lambda (ids) 
        (map (lambda (id) (hashtable-ref id->path-map id #f)) ids))
      shrinked-ids)))

(define (shrink-ids matrix ids result)
  (if (null? ids)
    result
    (let* ([current-from (car ids)]
        [rest-from (cdr ids)])
      (if (null? result)
        (shrink-ids matrix rest-from `((,current-from)))
        (if (zero? 
            (apply + 
              (map 
                (lambda (to) 
                  (matrix-take matrix current-from to)) 
                (car (reverse result)))))
          (shrink-ids 
            matrix
            rest-from 
            (append 
              (reverse (cdr (reverse result))) 
              (list (append (car (reverse result)) `(,current-from)))))
          (shrink-ids 
            matrix
            rest-from 
            (append 
              result
              (list `(,current-from)))))))))
)