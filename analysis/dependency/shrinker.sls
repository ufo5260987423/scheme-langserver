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
      [shrinked-ids (shrink-ids (file-linkage-matrix linkage) ids)])
    (map 
      (lambda (ids) 
        (map (lambda (id) (hashtable-ref id->path-map id #f)) ids))
      shrinked-ids)))

(define (shrink-ids matrix ids)
  (fold-left
    (lambda (prev current-from)
      (cond 
        [(null? prev) `((,current-from))]
        [(zero? 
          (apply + 
            (map 
              (lambda (to) 
                (matrix-take matrix current-from to)) 
              (car (reverse prev)))))
          (append 
            (reverse (cdr (reverse prev))) 
            (list (append (car (reverse prev)) `(,current-from))))]
        [else (append prev (list `(,current-from)))]))
    '()
    ids))
)