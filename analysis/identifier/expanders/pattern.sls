(library (scheme-langserver analysis identifier expanders pattern)
  (export
    make-pattern
    pattern?
    pattern-type
    pattern-content
    pattern-children
    pattern-parent
    pattern-exposed-literals
    gather-context

    expansion->printable-object
    pattern+index-node->bindings
    expand->index-node-compound-list)
  (import
    (chezscheme)
    (scheme-langserver util contain)
    (scheme-langserver util dedupe)
    (scheme-langserver util sub-list)

    (scheme-langserver virtual-file-system index-node))

;R6RS syntax-rules and syntax-case mainly consist of patterns and templates.
;Since scheme-langserver faces incomplete code, some fault-tolerant features
;are necessary:
;First, patterns are supposed to be strictly correct.
;Second, templates are not supposed to be strictly correct and this gives
;some tolerance. Or in other words, though R6RS says the template is
;literally a pattern, here we can't just do so.

(define-record-type pattern
  (fields
    ;r6rs 11.19
    ;underscore/pattern-variable/literal-identifier/list-form/pair-form/ellipse-list-form/ellipse-pair-form/vector-form/ellipse-vector-form/equal?-datum
    (immutable type)
    (immutable content)
    (immutable children)
    (mutable parent)
    (mutable exposed-literals)
    (mutable cdr?))
  (protocol
    (lambda (new)
      (lambda (successed-matched-pattern-expression)
        (cond
          [(eq? successed-matched-pattern-expression '_) (new 'underscore successed-matched-pattern-expression '() #f '() #f)]
          [(eq? successed-matched-pattern-expression '...) (new 'ellipse successed-matched-pattern-expression '() #f '() #f)]
          [(symbol? successed-matched-pattern-expression) (new 'pattern-variable/literal-identifier successed-matched-pattern-expression '() #f `(,successed-matched-pattern-expression) #f)]

          [(list? successed-matched-pattern-expression)
            (let ([p
                  (new
                    (if (contain? successed-matched-pattern-expression '...) 'ellipse-list-form 'list-form)
                    successed-matched-pattern-expression
                    (map make-pattern successed-matched-pattern-expression)
                    #f
                    '()
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              p)]
          [(pair? successed-matched-pattern-expression)
            (let ([p
                  (new
                    (let loop ([rest successed-matched-pattern-expression])
                      (if (pair? rest)
                        (if (eq? '... (car rest))
                          'ellipse-pair-form
                          (loop (cdr rest)))
                        'pair-form))
                    successed-matched-pattern-expression
                    (let loop ([rest successed-matched-pattern-expression])
                      (if (pair? rest)
                        `(,(make-pattern (car rest)) . ,(loop (cdr rest)))
                        `(,(make-pattern rest))))
                    #f
                    '()
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              (let ([children (pattern-children p)])
                (if (not (null? children))
                  (pattern-cdr?-set! (car (reverse children)) #t)))
              p)]
          [(vector? successed-matched-pattern-expression)
            (let ([p
                  (new
                    (if (contain? (vector->list successed-matched-pattern-expression) '...) 'ellipse-vector-form 'vector-form)
                    successed-matched-pattern-expression
                    (map make-pattern (vector->list successed-matched-pattern-expression))
                    #f
                    '()
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              p)]
          [else (new 'equal?-datum successed-matched-pattern-expression '() #f '() #f)])))))

(define (expansion->printable-object expansion)
  (cond
    [(list? expansion) (map expansion->printable-object expansion)]
    [(vector? expansion) (vector-map expansion->printable-object expansion)]
    [(pair? expansion)
      `(,(expansion->printable-object (car expansion)) . ,(expansion->printable-object (cdr expansion)))]
    [(index-node? expansion)
      (annotation-stripped (index-node-datum/annotations expansion))]
    [else expansion]))


(define (expand->index-node-compound-list template-pattern bindings pattern-context)
  (let* ([type (pattern-type template-pattern)]
      [content (pattern-content template-pattern)]
      [children (pattern-children template-pattern)])
    (case type
      [pattern-variable/literal-identifier
        (let ([pre-result (assq content bindings)])
          (if pre-result
            (if (eq? 'have-no-such-pattern-reference (cdr pre-result))
              content
              (cdr pre-result))
            content))]
      [(list-form vector-form pair-form)
        ((case type
            [list-form (lambda (a) a)]
            [vector-form list->vector]
            [pair-form (lambda (a)
              (let ([last (car (reverse a))])
                (if (and (list? last) (not (null? last)))
                  (append (reverse (cdr (reverse a))) last)
                  (let loop ([rest a])
                    (if (null? (cdr rest))
                      (car rest)
                      (cons (car rest) (loop (cdr rest))))))))])
          (map (lambda (c) (expand->index-node-compound-list c bindings pattern-context)) children))]
      [(ellipse-list-form ellipse-vector-form ellipse-pair-form)
        ((case type
            [ellipse-list-form (lambda (a) a)]
            [ellipse-vector-form list->vector]
            [ellipse-pair-form (lambda (a) `(,(reverse (cdr (reverse a))) . ,(car (reverse a))))])
          (let* ([max-i (length children)]
              [children-vec (list->vector children)])
            (let loop ([i 0])
              (cond
                [(>= i max-i) '()]
                [(and
                    (< (+ 1 i) max-i)
                    (eq? 'ellipse (pattern-type (vector-ref children-vec (+ 1 i)))))
                  (let* ([current-template-pattern (vector-ref children-vec i)]
                      [template-ellipsed-level
                        (let curr-loop ([j 1])
                          (if (and
                              (< (+ 1 i j) max-i)
                              (eq? 'ellipse (pattern-type (vector-ref children-vec (+ 1 i j)))))
                            (curr-loop (+ 1 j))
                            j))]
                      [exposed-literals (pattern-exposed-literals current-template-pattern)]
                      [pre-new-bindings (filter (lambda (p) (contain? exposed-literals (car p))) bindings)]
                      [new-bindings-list (private:bindings-product->list template-ellipsed-level pre-new-bindings pattern-context)])
                    `(,@(map (lambda (c) (expand->index-node-compound-list (vector-ref children-vec i) c pattern-context)) new-bindings-list) . ,(loop (+ 1 template-ellipsed-level i))))]
                [else
                  (if (eq? 'ellipse (pattern-type (vector-ref children-vec i)))
                    (loop (+ 1 i))
                    `(,(expand->index-node-compound-list (vector-ref children-vec i) bindings pattern-context) . ,(loop (+ 1 i))))]))))]
      [else (raise 'illegal-template)])))

;one expansion round per ellipsis level: bindings-product distributes the
;ellipsed variables' value lists over template-ellipsed-level rounds
(define (private:bindings-product->list times bindings pattern-context)
  (if (zero? times)
    bindings
    (map
      (lambda (new-bindings) (private:bindings-product->list (- times 1) new-bindings pattern-context))
      (fold-left
        (lambda (left right)
          (cond
            [(null? left) right]
            [(= 1 (length left))
              (map
                (lambda (r) (append (car left) r))
                right)]
            [(= 1 (length right))
              (map
                (lambda (l) (append l (car right)))
                left)]
            [else (map append left right)]))
        '()
        (map
          (lambda (kv-pair)
            (let* ([k (car kv-pair)]
                [vs (cdr kv-pair)]
                [pre-origin-pattern (assoc k pattern-context)]
                [origin-pattern (if pre-origin-pattern (cdr pre-origin-pattern) #f)]
                [cdr? (if origin-pattern (pattern-cdr? origin-pattern) #f)]
                [split? (and cdr? (find index-node? vs))])
              (if (and (list? vs) (not split?))
                (map (lambda (v) `((,k . ,v))) vs)
                `((,kv-pair)))))
          bindings)))))

;Chez-style single-pass matcher (cf. Chez s/syntax.ss $syntax-dispatch):
;walks the call-site index-node tree once, collecting bindings directly.
;pattern variables are distinguished from literals via the literals list
;(available at macro definition time).
;Returns: bindings alist ((var . value) ...), where value is an index-node,
;a list of index-nodes (ellipsed), or nested lists; #f if not matched;
;the symbol 'fallback if the pattern shape exceeds this matcher's coverage
;(the caller treats it as no-match and declines the expansion).
;MATCHER_DEBUG=1 in the environment prints mismatching patterns on the
;mismatch path (a matcher/Chez disagreement is always worth investigating).
(define (private:return-fallback)
  'fallback)

(define (pattern+index-node->bindings pattern index-node literals)
  (let ([r (private:match-pattern pattern index-node literals '())])
    (cond
      [(eq? r 'fallback) 'fallback]
      [(not r)
        (when (getenv "MATCHER_DEBUG")
          (display "MISMATCH pattern: ")
          (write (pattern-content pattern))
          (display " literal-count: ")
          (write (length (if (list? literals) literals '())))
          (newline))
        #f]
      [else r])))

(define (private:empty-node)
  (make-index-node '() '() '() '() #f '() '() '() '()))

(define (private:node-datum node)
  (guard (e [else '()])
    (annotation-stripped (index-node-datum/annotations node))))

(define (private:match-pattern pattern node literals bindings)
  (case (pattern-type pattern)
    [(underscore ellipse) bindings]
    [(pattern-variable/literal-identifier)
      (let ([sym (pattern-content pattern)])
        (if (memq sym literals)
          (and (eq? sym (private:node-datum node)) bindings)
          (cons (cons sym node) bindings)))]
    [(equal?-datum)
      (and (equal? (pattern-content pattern) (private:node-datum node)) bindings)]
    [(list-form)
      (private:match-list-like pattern node literals bindings)]
    [(vector-form)
      (private:match-children (pattern-children pattern) (index-node-children node) literals bindings)]
    [(pair-form)
      (private:match-pair-children (pattern-children pattern) (index-node-children node) literals bindings)]
    [(ellipse-list-form ellipse-vector-form)
      (private:match-ellipse (pattern-children pattern) (index-node-children node) literals bindings)]
    [(ellipse-pair-form)
      ;(P ... . tail) and its multi-ellipsis generalizations: the improper
      ;tail is a final fixed segment, so normalize the children to a proper
      ;segment list and share the list-form machinery (Chez treats the tail
      ;as one more pattern after the ellipsized ones)
      (private:match-ellipse (private:improper->proper (pattern-children pattern)) (index-node-children node) literals bindings)]
    [else (private:return-fallback)]))

;index-node represents quoted forms (both 'x and (quote x) in source) with
;datum (quote x) but WITHOUT the quote head among the children (tokenizer
;abbreviation convention). A (quote p) pattern whose first position is the
;literal quote therefore cannot use the default pairwise matching; bind the
;second pattern position to a synthesized content node instead.
(define (private:match-list-like pattern node literals bindings)
  (let ([kids (pattern-children pattern)])
    (if (and (= 2 (length kids))
          (eq? 'pattern-variable/literal-identifier (pattern-type (car kids)))
          (eq? 'quote (pattern-content (car kids)))
          (memq 'quote literals)
          (private:quoted-node? node))
      (private:match-pattern (cadr kids) (private:quote-content-node node) literals bindings)
      (private:match-children kids (index-node-children node) literals bindings))))

(define (private:quoted-node? node)
  (let ([datum (private:node-datum node)]
      [children (index-node-children node)])
    (and (pair? datum)
      (eq? 'quote (car datum))
      (or (null? children)
        (not (eq? 'quote (private:node-datum (car children))))))))

(define (private:quote-content-node node)
  (let* ([datum (private:node-datum node)]
      [content (cadr datum)]
      [source
        (or (guard (e [else #f]) (annotation-source (index-node-datum/annotations node)))
          (guard (e [else #f])
            (annotation-source (index-node-datum/annotations (index-node-parent node))))
          (make-source-object
            (make-source-file-descriptor "quote-content" (open-bytevector-input-port #vu8()))
            0 0))]
      [annotation (make-annotation content source content)])
    (make-index-node (index-node-parent node)
      (index-node-start node)
      (index-node-end node)
      annotation
      #f
      '() '() '() '())))

(define (private:match-children patterns nodes literals bindings)
  ;fixed-length pairwise; exhausted nodes get empty placeholder nodes (fault tolerance)
  (let loop ([ps patterns] [ns nodes] [b bindings])
    (cond
      [(null? ps) b]
      [(null? ns)
        (let ([r (private:match-pattern (car ps) (private:empty-node) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr ps) '() r)]))]
      [else
        (let ([r (private:match-pattern (car ps) (car ns) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr ps) (cdr ns) r)]))])))

(define (private:improper->proper improper)
  (let loop ([rest improper] [acc '()])
    (cond
      [(null? rest) (reverse acc)]
      [(pair? rest) (loop (cdr rest) (cons (car rest) acc))]
      [else (reverse (cons rest acc))])))

(define (private:match-pair-children improper-patterns nodes literals bindings)
  ;children of a pair-form: improper pattern list whose last element has cdr? #t
  ;and consumes the remaining nodes as a whole (single node if exactly one left,
  ;a list of nodes if more, an empty placeholder node if none)
  (let ([patterns (private:improper->proper improper-patterns)])
    (let loop ([ps patterns] [ns nodes] [b bindings])
      (cond
        [(null? ps) b]
        [(null? (cdr ps))
          (cond
            [(null? ns) (private:match-pattern (car ps) (private:empty-node) literals b)]
            [else (private:match-rest (car ps) ns literals b)])]
        [(null? ns)
          (let ([r (private:match-pattern (car ps) (private:empty-node) literals b)])
            (cond
              [(eq? r 'fallback) 'fallback]
              [(not r) #f]
              [else (loop (cdr ps) '() r)]))]
        [else
          (let ([r (private:match-pattern (car ps) (car ns) literals b)])
            (cond
              [(eq? r 'fallback) 'fallback]
              [(not r) #f]
              [else (loop (cdr ps) (cdr ns) r)]))]))))

(define (private:match-rest pattern nodes literals bindings)
  ;match a cdr-position pattern against a list of remaining nodes
  (case (pattern-type pattern)
    [(underscore ellipse) bindings]
    [(pattern-variable/literal-identifier)
      (let ([sym (pattern-content pattern)])
        (if (memq sym literals)
          (and (null? nodes) bindings)
          (cons (cons sym (if (null? (cdr nodes)) (car nodes) nodes)) bindings)))]
    [(list-form vector-form)
      (private:match-children (pattern-children pattern) nodes literals bindings)]
    [(ellipse-list-form ellipse-vector-form)
      (private:match-ellipse (pattern-children pattern) nodes literals bindings)]
    [else (private:return-fallback)]))

(define (private:split-ellipse-segments children)
  ;split children into ((pattern . ellipsis-count) ...); consecutive ellipses
  ;merge into the preceding pattern's count (ellipsis depth)
  (let loop ([cs children] [acc '()])
    (cond
      [(null? cs) (reverse acc)]
      [(eq? 'ellipse (pattern-type (car cs))) (loop (cdr cs) acc)]
      [else
        (let count-loop ([rest (cdr cs)] [n 0])
          (if (and (not (null? rest)) (eq? 'ellipse (pattern-type (car rest))))
            (count-loop (cdr rest) (+ 1 n))
            (loop rest (cons (cons (car cs) n) acc))))])))

;ELLIPSIS, MULTIPLE SEPARATE: R6RS requires patterns followed by ellipses in
;the same list to match lists of EQUAL length (Chez match-each+ semantics).
;Handled here when the ellipsed segments are contiguous (fixed segments only
;before the first / after the last); interleaved fixed segments defer to the
;legacy path.
(define (private:match-ellipse children nodes literals bindings)
  (let* ([segments (private:split-ellipse-segments children)]
      [ellipsed (filter (lambda (s) (> (cdr s) 0)) segments)])
    (cond
      [(null? (cdr ellipsed))
        (private:match-ellipse-segments (reverse segments) (reverse nodes) literals bindings)]
      [(private:ellipsed-contiguous? segments)
        (private:match-multi-ellipse segments nodes literals bindings)]
      [else (private:return-fallback)])))

(define (private:ellipsed-contiguous? segments)
  ;#t iff fixed segments appear only before the first ellipsed one or after
  ;the last one (flags form F* T+ F*)
  (let loop ([rest segments] [state 0])
    (cond
      [(null? rest) #t]
      [(> (cdar rest) 0) (and (<= state 1) (loop (cdr rest) 1))]
      [(= state 1) (loop (cdr rest) 2)]
      [else (loop (cdr rest) state)])))

(define (private:take-n lst n)
  (if (or (zero? n) (null? lst))
    '()
    (cons (car lst) (private:take-n (cdr lst) (- n 1)))))

(define (private:split-at lst n)
  (values (private:take-n lst n) (list-tail lst (min n (length lst)))))

(define (private:match-multi-ellipse segments nodes literals bindings)
  (let* ([flags (map (lambda (s) (> (cdr s) 0)) segments)]
      [k (length (filter (lambda (f) f) flags))]
      [prefix-count
        (let loop ([fs flags] [n 0])
          (if (or (null? fs) (car fs)) n (loop (cdr fs) (+ n 1))))]
      [suffix-count
        (let loop ([fs (reverse flags)] [n 0])
          (if (or (null? fs) (car fs)) n (loop (cdr fs) (+ n 1))))]
      [node-count (length nodes)]
      [middle-count (- node-count prefix-count suffix-count)])
    (if (or (< middle-count 0) (not (zero? (modulo middle-count k))))
      (private:return-fallback)
      (let ([block-size (quotient middle-count k)])
        (let*-values
          ([(prefix-segments rest-segments)
            (private:split-at segments prefix-count)]
          [(rev-middle-segments suffix-segments)
            ;rs keeps the middle segments in reverse order; srev holds the
            ;trailing fixed (suffix) segments reversed
            (let loop ([rs (reverse rest-segments)] [srev '()])
              (if (and (not (null? rs)) (= 0 (cdar rs)))
                (loop (cdr rs) (cons (car rs) srev))
                (values rs (reverse srev))))]
          [(prefix-nodes rest-nodes)
            (private:split-at nodes prefix-count)]
          [(middle-nodes suffix-nodes)
            (private:split-at rest-nodes (- (length rest-nodes) suffix-count))])
          (let ([b0 (private:match-prefix (reverse prefix-segments) prefix-nodes literals bindings)])
            (cond
              [(eq? b0 'fallback) 'fallback]
              [(not b0) #f]
              [else
                ;middle segments are matched in ORIGINAL left-to-right order,
                ;each taking the next block-size nodes
                (let loop ([segs (reverse rev-middle-segments)] [ns middle-nodes] [b b0])
                  (if (null? segs)
                    (private:match-prefix (reverse suffix-segments) suffix-nodes literals b)
                    (let* ([segment-pattern (caar segs)]
                        [block (private:take-n ns block-size)]
                        [rest-ns (list-tail ns (min block-size (length ns)))]
                        [results (map (lambda (n) (private:match-pattern segment-pattern n literals '())) block)])
                      (cond
                        [(member 'fallback results) (private:return-fallback)]
                        [(member #f results) #f]
                        [else
                          (let* ([vars (pattern-exposed-literals segment-pattern)]
                              [collected
                                (map
                                  (lambda (v) (cons v (map (lambda (rb) (cdr (assq v rb))) results)))
                                  vars)])
                            (loop (cdr segs) rest-ns (append collected b)))]))))])))))))

(define (private:match-ellipse-segments rev-segments rev-nodes literals bindings)
  ;walk segments right-to-left; rev-nodes is the reversed node children.
  ;The (single) ellipsed segment collects bindings from ALL remaining nodes
  ;in original left-to-right order (nested ellipsis recurses per node).
  (cond
    [(null? rev-segments) bindings]
    [(> (cdar rev-segments) 0)
      ;the ellipsed segment consumes only the nodes LEFT AFTER the prefix
      ;segments take theirs (one node per fixed segment)
      (let* ([ordered (reverse rev-nodes)]
          [prefix-segments (cdr rev-segments)]
          [prefix-count (length prefix-segments)]
          [block (list-tail ordered (min prefix-count (length ordered)))]
          [results (map (lambda (n) (private:match-pattern (caar rev-segments) n literals '())) block)])
        (cond
          [(member 'fallback results) (private:return-fallback)]
          [(member #f results) #f]
          [else
            (let* ([vars (pattern-exposed-literals (caar rev-segments))]
                [collected
                  (map
                    (lambda (v) (cons v (map (lambda (rb) (cdr (assq v rb))) results)))
                    vars)]
                [new-bindings (append collected bindings)])
              (private:match-prefix prefix-segments ordered literals new-bindings))]))]
    [(null? rev-nodes)
      (let ([r (private:match-pattern (caar rev-segments) (private:empty-node) literals bindings)])
        (cond
          [(eq? r 'fallback) 'fallback]
          [(not r) #f]
          [else (private:match-ellipse-segments (cdr rev-segments) '() literals r)]))]
    [else
      (let ([r (private:match-pattern (caar rev-segments) (car rev-nodes) literals bindings)])
        (cond
          [(eq? r 'fallback) 'fallback]
          [(not r) #f]
          [else (private:match-ellipse-segments (cdr rev-segments) (cdr rev-nodes) literals r)]))]))

(define (private:match-prefix rev-prefix-segments ordered literals bindings)
  ;fixed segments left of the ellipsed one: pairwise against the first nodes
  (let loop ([segs (reverse rev-prefix-segments)] [ns ordered] [b bindings])
    (cond
      [(null? segs) b]
      [(null? ns)
        (let ([r (private:match-pattern (caar segs) (private:empty-node) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr segs) '() r)]))]
      [else
        (let ([r (private:match-pattern (caar segs) (car ns) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr segs) (cdr ns) r)]))])))

(define (gather-context pattern)
  (case (pattern-type pattern)
    [pattern-variable/literal-identifier `((,(pattern-content pattern) . ,pattern))]
    [(ellipse-list-form list-form ellipse-vector-form vector-form ellipse-pair-form pair-form)
      (apply append (map gather-context (pattern-children pattern)))]
    [else '()]))
)
