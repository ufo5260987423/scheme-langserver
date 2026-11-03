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
;;   (b) (apply f x ... rest): the elastic-tail element type of f's
;;       signature -- the item the trailing .../**1 attaches to,
;;       per syntax-candy's segment semantics -- constrains every
;;       element rest supplies beyond the explicit arguments.
;;
;; Multiple constraints union (deduped); unsolved/something? candidates
;; are dropped, and when nothing concrete remains the document is left
;; exactly as before. Scope: the define-function form only; lambda /
;; case-lambda rest and car/map-style usage inference are future work.
;; Rewrites REPLACE the rest template inside existing arrows (never
;; append new substitutions), and any failure degrades to status quo.

(define PRIVATE-MAX-REST-ELEMENTS-MS-PER-DOCUMENT 50)
(define PRIVATE-MAX-REST-ELEMENT-CANDIDATES 8)

(define private:budget-tag 'private:rest-elements-budget)

(define (private:now-ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t)) (div (time-nanosecond t) 1000000))))

(define (private:check-budget! deadline)
  (when (> (private:now-ms) deadline)
    (raise private:budget-tag)))

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
      (for-each
        (lambda (target) (private:solve-target! document target deadline))
        (private:collect-rest-defines document)))))

;;target = (name-node . fixed-count); produced only for define-function
;;forms with DOTTED formals (the . rest part), including the (name .
;;args) zero-fixed shape. Proper-list formals have no rest; variable
;;defines and curried heads are out of scope.
(define (private:collect-rest-defines document)
  (let walk ([nodes (document-index-node-list document)] [acc '()])
    (if (null? nodes)
      acc
      (let* ([node (dereference-index-node (car nodes))]
          [children (if (index-node? node) (index-node-children node) '())]
          [head
            (and (not (null? children))
              (let ([e (annotation-stripped (index-node-datum/annotations (car children)))])
                (and (symbol? e) e)))])
        (cond
          [(and (eq? 'define head) (>= (length children) 3))
            (let* ([formals-node (cadr children)]
                [formals (annotation-stripped (index-node-datum/annotations formals-node))]
                [formals-children (index-node-children formals-node)]
                [name-node (and (pair? formals-children) (car formals-children))])
              (walk (append children (cdr nodes))
                (if (and (pair? formals)
                      (not (list? formals))
                      (symbol? (car formals))
                      (index-node? name-node))
                  ;;children = [name-symbol fixed... rest-symbol]; the
                  ;;improper formals guarantee a trailing rest symbol
                  (cons (cons name-node (- (length formals-children) 2)) acc)
                  acc)))]
          [(index-node? node)
            (walk (append children (cdr nodes)) acc)]
          [else
            (walk (cdr nodes) acc)])))))

(define (private:solve-target! document target deadline)
  (when (< (private:now-ms) deadline)
    ;;per-target degrade: a pathology in one function must not stop the
    ;;others
    (guard (ex [else (void)])
      (let* ([name-node (car target)]
          [fixed-count (cdr target)]
          [candidates
            (private:dedupe-atoms
              (append
                (private:call-site-elements document name-node fixed-count deadline)
                (private:apply-elements document name-node fixed-count deadline)))])
        (when (and (not (null? candidates))
                (<= (length candidates) PRIVATE-MAX-REST-ELEMENT-CANDIDATES))
          (private:rewrite-arrows! name-node candidates))))))

;;source (a): every call to the function contributes the interpreted
;;types of the arguments beyond the fixed prefix. The walk descends the
;;whole document; a node is a call when its head child is a leaf symbol
;;resolving to this define's name node.
(define (private:call-site-elements document name-node fixed-count deadline)
  (let walk ([nodes (document-index-node-list document)] [acc '()])
    (cond
      [(null? nodes) acc]
      [else
        (private:check-budget! deadline)
        (let* ([node (dereference-index-node (car nodes))]
            [children (if (index-node? node) (index-node-children node) '())])
          (cond
            [(and (not (null? children))
                (private:head-resolves-to? document node (car children) name-node))
              (walk (append children (cdr nodes))
                (append
                  (private:interpret-arg-list
                    (list-tail children (min (+ 1 fixed-count) (length children))))
                  acc))]
            [(index-node? node)
              (walk (append children (cdr nodes)) acc)]
            [else
              (walk (cdr nodes) acc)]))])))

(define (private:head-resolves-to? document call-node head-child name-node)
  (let ([head-expr (annotation-stripped (index-node-datum/annotations head-child))])
    (and (symbol? head-expr)
      (find
        (lambda (ref) (eq? name-node (identifier-reference-index-node ref)))
        (find-available-references-for document call-node head-expr)))))

;;interpret each argument node and keep only solved, non-something?
;;atoms -- unsolved arguments carry no element information
(define (private:interpret-arg-list arg-nodes)
  (apply append
    (map
      (lambda (node)
        (filter
          (lambda (c) (not (eq? 'something? c)))
          (filter type:solved? (type:interpret-result-list node))))
      arg-nodes)))

;;source (b): (apply f x ... rest) -- the last child is the rest list;
;;f's signature elastic-tail element type constrains every element rest
;;supplies beyond the explicit arguments.
(define (private:apply-elements document name-node fixed-count deadline)
  (let walk ([nodes (document-index-node-list document)] [acc '()])
    (cond
      [(null? nodes) acc]
      [else
        (private:check-budget! deadline)
        (let* ([node (dereference-index-node (car nodes))]
            [children (if (index-node? node) (index-node-children node) '())]
            [head
              (and (not (null? children))
                (let ([e (annotation-stripped (index-node-datum/annotations (car children)))])
                  (and (symbol? e) e)))])
          (cond
            [(and (eq? 'apply head) (>= (length children) 3))
              (let ([tail-elements (private:apply-tail-elements document node children)])
                (walk (append children (cdr nodes)) (append tail-elements acc)))]
            [(index-node? node)
              (walk (append children (cdr nodes)) acc)]
            [else
              (walk (cdr nodes) acc)]))])))

(define (private:apply-tail-elements document call-node children)
  (let* ([f-child (cadr children)]
      [f-expr (annotation-stripped (index-node-datum/annotations f-child))])
    (if (symbol? f-expr)
      (apply append
        (map
          (lambda (ref) (private:arrow-tail-elements ref))
          (find-available-references-for document call-node f-expr)))
      '())))

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

;;write-back: arrows on the carrier (name) node whose parameter
;;template has an elastic something? tail -- the rest position the
;;generator emits as (inner:list? fixed... something? ...) -- get that
;;tail item replaced by one arrow per element candidate; every other
;;substitution (usage arrows from trivial.sls, fixed-arity arrows, raw
;;remainders) passes through untouched.
(define (private:rewrite-arrows! name-node candidates)
  (let ([substitutions (index-node-substitution-list name-node)])
    (index-node-substitution-list-set! name-node
      (apply append
        (map
          (lambda (item) (private:rewrite-arrow item candidates))
          substitutions)))))

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
)
