(library (scheme-langserver analysis type substitutions rest-elements)
  (export rest-elements-substitutions-for!)
  (import
    (chezscheme)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver util dedupe)

    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker))

;; P3: dotted-rest element-type inference. The generator emits a rest
;; parameter as an elastic something? tail -- (inner:list? fixed...
;; something? ...), the variadic-tail encoding of syntax-candy -- so
;; hover/export show "rest arguments: unknown" for any variadic
;; function. This post-pass (run after the fixpoint pass, with the same
;; guard/budget discipline) collects element constraints from two
;; sources and rewrites the elastic tail item on the carrier node's
;; arrows:
;;
;;   (a) call sites in the same document: for (define (name fixed...
;;       . rest) ...), every call (name fixed-vals... extras...)
;;       contributes the interpreted types of the extra arguments;
;;   (b) (apply f x ... rest) whose last argument IS this function's
;;       rest parameter: the elastic-tail element type of f's
;;       signature -- the item the trailing .../**1 attaches to,
;;       per syntax-candy's segment semantics -- constrains every
;;       element rest supplies beyond the explicit arguments.
;;
;; Multiple constraints union; unsolved/something? candidates are
;; dropped, and when nothing concrete remains the document is left
;; exactly as before. Scope: the define-function form only; lambda /
;; case-lambda rest and car/map-style usage inference are future work.
;; Rewrites REPLACE the rest template inside existing arrows (never
;; append new substitutions), and any failure degrades to status quo.
;;
;; Performance contract (all measured on the dogfood workspace):
;; ONE document walk collects every symbol-headed application shape
;; (facts); per-target work only resolves candidates whose symbol
;; matches. The wall-clock budget (50 ms/document, same as the
;; fixpoint pass) is checked between facts and between argument
;; interpretations. On top of it, three static/fused guards bound the
;; pathologies that a between-calls budget cannot reach:
;;   1. argument weight: an argument is interpreted only when its
;;      reachable subtree (nodes + substitution lists) is small --
;;      interpretation expands substitutions through references that
;;      no local size hint can see;
;;   2. candidate-list cycle/length cap BEFORE append: interpreter
;;      output can be CIRCULAR (cyclic type expressions survive from
;;      the substitution layer), on which plain append/length would
;;      spin forever (measured: 14.3 s in a single target of
;;      srfi-115's regexp-impl.scm);
;;   3. visited-set DFS: shared/cyclic AST nodes are walked once
;;      (the #0/#1-literal plague, cf. AGENTS.md and fixpoint.sls).

(define PRIVATE-MAX-REST-ELEMENTS-MS-PER-DOCUMENT 50)
(define PRIVATE-MAX-REST-ELEMENT-CANDIDATES 8)
(define PRIVATE-MAX-ARG-SUBTREE-NODES 300)
(define PRIVATE-MAX-ARG-SUBSTITUTIONS 32)

(define private:budget-tag 'private:rest-elements-budget)

(define (private:now-ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t)) (div (time-nanosecond t) 1000000))))

(define (private:check-budget! deadline)
  (when (> (private:now-ms) deadline)
    (raise private:budget-tag)))

;;returns the length when the list is proper and shorter than cap; #f
;;on a cycle, an improper tail, or a list longer than cap -- all of
;;which mean "skip this target" (the status-quo semantics)
(define (private:bounded-length items cap)
  (let loop ([rest items] [n 0])
    (cond
      [(null? rest) n]
      [(or (>= n cap) (not (pair? rest))) #f]
      [else (loop (cdr rest) (+ n 1))])))

(define (private:measure-arg node visited acc)
  (cond
    [(>= acc PRIVATE-MAX-ARG-SUBTREE-NODES) acc]
    [(not (index-node? node)) acc]
    [(hashtable-ref visited node #f) acc]
    [else
      (hashtable-set! visited node #t)
      (let ([with-subs
              (+ (+ acc 1)
                (min (length (index-node-substitution-list node))
                  PRIVATE-MAX-ARG-SUBSTITUTIONS))])
        (fold-left
          (lambda (a child)
            (private:measure-arg child visited a))
          with-subs
          (index-node-children node)))]))

(define (private:arg-node-light-enough? node)
  (< (private:measure-arg node (make-eq-hashtable) 0)
    PRIVATE-MAX-ARG-SUBTREE-NODES))

(define (private:filter-markers items)
  (filter
    (lambda (x) (and (not (eq? x '...)) (not (eq? x '**1))))
    items))

(define (private:drop-last items)
  (if (null? items)
    '()
    (reverse (cdr (reverse items)))))

(define (private:dedupe-atoms atoms)
  (dedupe-deduped '() atoms))

(define (private:node-resolves-to? document node symbol target-node)
  (find
    (lambda (ref) (eq? target-node (identifier-reference-index-node ref)))
    (find-available-references-for document node symbol)))

;;interpret each argument node and keep only solved, non-something?
;;atoms -- unsolved arguments carry no element information
(define (private:interpret-arg-list arg-nodes deadline)
  (apply append
    (map
      (lambda (node)
        (private:check-budget! deadline)
        (filter
          (lambda (c) (not (eq? 'something? c)))
          (filter type:solved? (type:interpret-result-list node))))
      (filter private:arg-node-light-enough? arg-nodes))))

;;source (a): facts whose head symbol matches the function name AND
;;resolve to its name node contribute the interpreted types of the
;;arguments beyond the fixed prefix.
(define (private:call-site-elements document name-node name-symbol fixed-count facts deadline)
  (apply append
    (map
      (lambda (fact)
        ;;budget: interpretation of a single argument can burn seconds
        ;;on pathological substitutions -- the pass must stay a pass
        (private:check-budget! deadline)
        (if (eq? (car fact) name-symbol)
          (let ([call-node (cadr fact)]
              [children (cddr fact)])
            (if (private:node-resolves-to? document call-node name-symbol name-node)
              (private:interpret-arg-list
                (list-tail children (min (+ 1 fixed-count) (length children)))
                deadline)
              '()))
          '()))
      facts)))

;;from one arrow of f's signature: if the parameter template has an
;;elastic tail (.../**1), the marker's attached item -- the last item
;;before the marker -- is the element type of every tail position.
(define (private:arrow-tail-elements ref)
  (apply append
    (map
      (lambda (arrow)
        (let* ([param (inner:lambda-param arrow)]
            [content
              (if (and (list? param) (eq? (car param) 'inner:list?))
                (inner:list-content param)
                '())]
            [items (private:filter-markers content)])
          (if (and (< (length items) (length content))
                (not (null? items)))
            (list (car (reverse items)))
            '())))
      (filter inner:lambda? (identifier-reference-type-expressions ref)))))

(define (private:apply-tail-elements document fact rest-node rest-symbol)
  (let* ([call-node (cadr fact)]
      [children (cddr fact)]
      [last-child (car (reverse children))]
      [last-expr (annotation-stripped (index-node-datum/annotations last-child))]
      [f-child (cadr children)]
      [f-expr (annotation-stripped (index-node-datum/annotations f-child))])
    (if (and (symbol? last-expr)
            (eq? last-expr rest-symbol)
            (private:node-resolves-to? document call-node last-expr rest-node)
            (symbol? f-expr))
      (apply append
        (map
          (lambda (ref) (private:arrow-tail-elements ref))
          (find-available-references-for document call-node f-expr)))
      '())))

;;source (b): (apply f x ... rest) whose LAST argument is THIS
;;target's rest parameter (symbol match + resolution) -- f's signature
;;elastic-tail element type constrains every element rest supplies
;;beyond the explicit arguments. The rest-ownership check matters:
;;without it every apply call in the document would contribute to
;;every variadic function (sound, but uselessly wide).
(define (private:apply-elements document rest-node rest-symbol facts deadline)
  (apply append
    (map
      (lambda (fact)
        (private:check-budget! deadline)
        (if (and (eq? (car fact) 'apply) (>= (length (cddr fact)) 3))
          (private:apply-tail-elements document fact rest-node rest-symbol)
          '()))
      facts)))

;;write-back for ONE substitution item: arrows whose parameter
;;template has an elastic something? tail -- the rest position the
;;generator emits as (inner:list? fixed... something? ...) -- get that
;;tail item replaced by one arrow per element candidate; every other
;;substitution (usage arrows from trivial.sls, fixed-arity arrows, raw
;;remainders) passes through untouched.
(define (private:rewrite-arrow item candidates)
  (cond
    [(not (inner:lambda? item)) (list item)]
    [else
      (let ([param (inner:lambda-param item)]
          [return (inner:lambda-return item)])
        (if (and (list? param) (eq? (car param) 'inner:list?))
          (let* ([content (inner:list-content param)]
              [items (private:filter-markers content)]
              [has-marker? (< (length items) (length content))])
            ;;rest shape: trailing marker whose attached item is the
            ;;something? sentinel (never touch a KNOWN tail type)
            (if (and has-marker?
                  (not (null? items))
                  (eq? 'something? (car (reverse items))))
              (let ([fixed (private:drop-last items)])
                (map
                  (lambda (element)
                    `(,return <- (inner:list? ,@fixed ,element ...)))
                  candidates))
              (list item)))
          (list item)))]))

(define (private:rewrite-arrows! name-node candidates)
  (let ([substitutions (index-node-substitution-list name-node)])
    (index-node-substitution-list-set! name-node
      (apply append
        (map
          (lambda (item) (private:rewrite-arrow item candidates))
          substitutions)))))

;;fact = (head-symbol node . children): every application-shaped node
;;with a leaf-symbol head, collected ONCE per document. Both constraint
;;sources filter these facts by symbol BEFORE resolving anything, so
;;the cost of a document without matching candidates is a single cheap
;;walk.
(define (private:collect-application-facts document deadline)
  (let walk ([nodes (document-index-node-list document)] [acc '()] [visited (make-eq-hashtable)])
    (cond
      [(null? nodes) acc]
      [else
        (private:check-budget! deadline)
        (let* ([node (dereference-index-node (car nodes))]
            [children
              (if (and (index-node? node) (not (hashtable-ref visited node #f)))
                (begin
                  (hashtable-set! visited node #t)
                  (index-node-children node))
                '())]
            [head
              (and (not (null? children))
                (let ([e (annotation-stripped (index-node-datum/annotations (car children)))])
                  (and (symbol? e) e)))])
          (walk (append children (cdr nodes))
            (if head
              (cons (cons head (cons node children)) acc)
              acc)
            visited))])))

;;target = (name-node name-symbol fixed-count rest-node rest-symbol);
;;produced only for define-function forms with DOTTED formals (the .
;;rest part), including the (name . args) zero-fixed shape.
;;Proper-list formals have no rest; variable defines and curried heads
;;are out of scope.
(define (private:collect-rest-defines document deadline)
  (let walk ([nodes (document-index-node-list document)] [acc '()] [visited (make-eq-hashtable)])
    (cond
      [(null? nodes) acc]
      [else
        (private:check-budget! deadline)
        (let* ([node (dereference-index-node (car nodes))]
            [children
              (if (and (index-node? node) (not (hashtable-ref visited node #f)))
                (begin
                  (hashtable-set! visited node #t)
                  (index-node-children node))
                '())]
            [head
              (and (not (null? children))
                (let ([e (annotation-stripped (index-node-datum/annotations (car children)))])
                  (and (symbol? e) e)))])
          (cond
            [(and (eq? 'define head) (>= (length children) 3))
              (let* ([formals-node (cadr children)]
                  [formals (annotation-stripped (index-node-datum/annotations formals-node))]
                  [formals-children (index-node-children formals-node)]
                  [name-node (and (pair? formals-children) (car formals-children))]
                  [rest-node (and (pair? formals-children) (car (reverse formals-children)))])
                (walk (append children (cdr nodes))
                  (if (and (pair? formals)
                        (not (list? formals))
                        (symbol? (car formals))
                        (index-node? name-node)
                        (index-node? rest-node))
                    ;;children = [name-symbol fixed... rest-symbol]; the
                    ;;improper formals guarantee a trailing rest symbol
                    (cons
                      (list name-node
                        (annotation-stripped (index-node-datum/annotations name-node))
                        (- (length formals-children) 2)
                        rest-node
                        (annotation-stripped (index-node-datum/annotations rest-node)))
                      acc)
                    acc)
                  visited))]
            [(index-node? node)
              (walk (append children (cdr nodes)) acc visited)]
            [else
              (walk (cdr nodes) acc visited)]))])))

(define (private:solve-target! document target facts deadline)
  (when (< (private:now-ms) deadline)
    ;;per-target degrade: a pathology in one function must not stop the
    ;;others
    (guard (ex [else (void)])
      (let* ([name-node (car target)]
          [name-symbol (cadr target)]
          [fixed-count (caddr target)]
          [rest-node (cadddr target)]
          [rest-symbol (car (cddddr target))]
          [from-calls
            (private:call-site-elements document name-node name-symbol fixed-count facts deadline)]
          [from-applies
            (private:apply-elements document rest-node rest-symbol facts deadline)])
        ;;cap BEFORE append: the candidate lists can be CIRCULAR or
        ;;gigantic (interpreter output is unbounded) and append/length
        ;;on one would spin; bounded-length breaks the cycle
        (let ([ok-calls (private:bounded-length from-calls 33)]
            [ok-applies (private:bounded-length from-applies 33)])
          (when (and ok-calls ok-applies)
            (let ([n (+ ok-calls ok-applies)])
              (when (<= n PRIVATE-MAX-REST-ELEMENT-CANDIDATES)
                (let ([candidates
                        (private:dedupe-atoms (append from-calls from-applies))])
                  (private:rewrite-arrows! name-node candidates))))))))))

(define (rest-elements-substitutions-for! document)
  (let ([deadline (+ (private:now-ms) PRIVATE-MAX-REST-ELEMENTS-MS-PER-DOCUMENT)])
    (guard (ex [else
                ;;never break the generator; a failed pass degrades to
                ;;status quo with a diagnosis (budget exhaustion is
                ;;silent)
                (unless (eq? ex private:budget-tag)
                  (append-new-diagnoses
                    document
                    `(0 0 2
                      ,(string-append "Rest-element inference warning: "
                          (if (condition? ex) (condition-message ex) "unknown"))
                      "type" "type-rest-elements-warning")))
                (void)])
      (let ([targets (private:collect-rest-defines document deadline)])
        (unless (null? targets)
          (let ([facts (private:collect-application-facts document deadline)])
            (for-each
              (lambda (target) (private:solve-target! document target facts deadline))
              targets)))))))
)
