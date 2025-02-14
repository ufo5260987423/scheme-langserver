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

(define (shrink-ids matrix ids advance-list)
  (let ([tmp 
      (filter
        (lambda (current-from) 
          (zero? 
            (apply + 
              (map 
                (lambda (to) (matrix-take matrix current-from to))
                ids))))
        ids)])
    (cond 
      [(null? ids) '()]
      [(null? tmp) 
        ;here, the ids is basically a super node representing cycles
        ;so, ramdom remove one to break
        (car 
          (sort 
            (lambda (a b)
              (< (length a) (length b)))
            (map 
              (lambda (id)
                (shrink-ids matrix (remove id ids) `(,@advance-list ,id)))
              ids)))]
      [else 
        `(,tmp 
          ,@(shrink-ids matrix 
              (fold-left
                (lambda (prev id)
                  (if (memq id tmp)
                    prev
                    `(,@prev ,id)))
                advance-list
                ids)
              '()))])))
)