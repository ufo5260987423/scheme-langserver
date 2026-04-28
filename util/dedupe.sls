(library (scheme-langserver util dedupe)
    (export 
        dedupe
        ordered-dedupe
        dedupe-deduped)
    (import 
        (rnrs)
        (scheme-langserver util contain))

;; Fast O(n) dedupe using a hash set when the equality predicate is `equal?`.
;; Falls back to the original O(n²) recursive filter for custom predicates.

(define (private:dedupe-hashtable e)
    (let ([ht (make-hashtable equal-hash equal?)]
          [result '()])
      (let loop ([items e])
        (if (null? items)
            (reverse result)
            (let ([item (car items)])
              (if (hashtable-contains? ht item)
                  (loop (cdr items))
                  (begin
                    (hashtable-set! ht item #t)
                    (set! result (cons item result))
                    (loop (cdr items)))))))))

(define dedupe
    (case-lambda
        [(e) (dedupe e equal?)]
        [(e equal-procedure) 
            (if (eq? equal-procedure equal?)
                (private:dedupe-hashtable e)
                (if (null? e) 
                    '()
                    (cons 
                        (car e) 
                        (dedupe 
                            (filter 
                                (lambda (x) (not (equal-procedure x (car e)))) 
                                (cdr e))
                            equal-procedure))))]))

(define ordered-dedupe
    (case-lambda 
        [(e) (ordered-dedupe e equal?)]
        [(e equal-procedure) 
            (cond 
                [(null? e) e]
                [(= 1 (length e)) e]
                [(equal-procedure (car e) (cadr e))
                    (ordered-dedupe (cdr e) equal-procedure)]
                [else
                    (cons 
                        (car e)
                        (ordered-dedupe (cdr e) equal-procedure))])]))

;; Fast O(n+m) dedupe-deduped using a hash set when the equality predicate is `equal?`.
;; Falls back to the original O(n·m) linear search for custom predicates.

(define (private:dedupe-deduped-hashtable e1 e2)
    (let ([ht (make-hashtable equal-hash equal?)])
      (for-each (lambda (x) (hashtable-set! ht x #t)) e1)
      (append e1
          (filter (lambda (x) (not (hashtable-contains? ht x))) 
              e2))))

(define dedupe-deduped
    (case-lambda 
        [(e1 e2) (dedupe-deduped e1 e2 equal?)]
        [(e1 e2 equal-procedure) 
            (cond 
                [(or (null? e1) (null? e2)) (append e1 e2)]
                [(< (length e1) (length e2)) (dedupe-deduped e2 e1 equal-procedure)]
                [(eq? equal-procedure equal?)
                    (private:dedupe-deduped-hashtable e1 e2)]
                [else
                    (append e1
                        (filter (lambda (x) (not (contain? e1 x equal-procedure))) 
                            e2))])]))
)
