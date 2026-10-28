(library (scheme-langserver analysis type substitutions macro-expansion)
  (export type:expand-and-infer
    type:expansion-retention-info)
  (import
    (chezscheme)

    (scheme-langserver util path)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier expander ir-auto-route))

;;; Macro type inference BY EXPANSION, mimicking the identifier side's
;;; automatic macro resolution: the type router's generic macro arm
;;; expands the call site with Chez's own expander, builds a VIRTUAL
;;; index-node document from the reconstructed annotated expansion (whose
;;; positions $uncprep strips -- chez-ir->annotated-sexp restores them
;;; from the IR, beta-reducing single-clause case-lambda applications
;;; into LETs so the generator's binding-to-init typing carries the
;;; value flow), seeds the references the generator needs (rnrs rule
;;; tables for library procedures, meta marks for special forms), runs
;;; the REAL type generator over the expansion, and maps the resulting
;;; substitutions back onto the original call site's nodes by source
;;; position. The expansion turns macro pattern semantics into ordinary
;;; code that the generator already types: no hand-written pattern
;;; knowledge, macro-agnostic by construction.
;;;
;;; run-generator (the generator's own construct-substitutions-for) is
;;; INJECTED by generator.sls: importing it here would close the cycle
;;; generator -> router -> this module -> generator.
;;;
;;; Back-propagated substitution entries may hold node pointers into the
;;; virtual tree, and interpretation is lazy and in-process (type
;;; substitutions are recomputed per init, never cached), so each virtual
;;; document must outlive the pass that produced it. Retention is
;;; therefore BOUNDED, not append-only: one virtual document per (real
;;; document, call-site span), REPLACE on re-derivation. Before this the
;;; table grew by one full virtual tree per refresh per call site
;;; (measured: +505 nodes each, GC-proof) because didChange mutates the
;;; document in place (document-text-set!, workspace.sls) and an entry
;;; keyed by the document object never expired. Invalidation is
;;; text-identity: the per-document entry pins the text object it was
;;; derived from; a text swap drops every stale span's virtual tree from
;;; BOTH tables. did-close needs no hook: documents stay in the file
;;; tree, so total retention is bounded by live documents x their macro
;;; call sites.

(define private:retention (make-eq-hashtable))
;;;real document -> (text-object . span-ht); span-ht: (start . end) ->
;;;virtual document
(define private:virtual-documents (make-eq-hashtable))

(define (private:drop-virtual! virtual-document)
  (hashtable-delete! private:virtual-documents virtual-document))

(define (type:expansion-retention-info)
  ;;observability for the bounded-retention invariant (used by the
  ;;memory-regression measurement): ((real-docs . n) (virtual-docs . n)
  ;;(span-entries . n)); virtual-docs must equal span-entries and stay
  ;;flat across refreshes
  (let ([span-total 0])
    (let-values ([(keys entries) (hashtable-entries private:retention)])
      (vector-for-each
        (lambda (entry) (set! span-total (+ span-total (hashtable-size (cdr entry)))))
        entries))
    `((real-docs . ,(hashtable-size private:retention))
      (virtual-docs . ,(hashtable-size private:virtual-documents))
      (span-entries . ,span-total))))

(define (private:retain! real-document call-start call-end virtual-document)
  (let* ([text (document-text real-document)]
      [entry (hashtable-ref private:retention real-document #f)]
      [span-ht
        (if (and entry (eq? text (car entry)))
          (cdr entry)
          (begin
            (when entry
              (vector-for-each private:drop-virtual! (hashtable-values (cdr entry))))
            (let ([ht (make-hashtable equal-hash equal?)])
              (hashtable-set! private:retention real-document (cons text ht))
              ht)))])
    (let ([previous (hashtable-ref span-ht (cons call-start call-end) #f)])
      (when previous
        (private:drop-virtual! previous))
      (hashtable-set! span-ht (cons call-start call-end) virtual-document))))

(define (private:copy-substitutions! virtual-node real-document path start end)
  ;;copy substitutions of expansion nodes that carry the call site's own
  ;;source positions onto the corresponding real document nodes; returns
  ;;the number of entries copied at this node (children NOT recursed)
  (let ([ann (index-node-datum/annotations virtual-node)])
    (if (and (annotation? ann)
            (let ([src (annotation-source ann)])
              (and (source-object? src)
                (string=? path (source-file-descriptor-path (source-object-sfd src))))))
      (let ([bias (source-object-bfp (annotation-source ann))])
        (when (getenv "EXPAND_DEBUG")
          (format #t "[bp] bias=~a subs=~a span=~a-~a~%"
            bias (length (index-node-substitution-list virtual-node)) start end))
        (if (and (>= bias start) (< bias end))
          (let* ([subs (index-node-substitution-list virtual-node)]
              [real-node (pick-index-node-from
                          (document-index-node-list real-document) bias)])
            (if (and (index-node? real-node) (pair? subs))
              (begin
                (for-each
                  (lambda (sub) (extend-index-node-substitution-list real-node sub))
                  subs)
                (length subs))
              0))
          0))
      0)))

(define private:bp-stats (make-eq-hashtable))
(define (private:bp-stat! key)
  (hashtable-set! private:bp-stats key (+ 1 (hashtable-ref private:bp-stats key 0))))

(define (private:back-propagate! virtual-node real-document path start end)
  ;;recursively copy; returns the total number of entries copied
  (private:bp-stat! 'nodes)
  (let ([ann (index-node-datum/annotations virtual-node)])
    (cond
      [(not (annotation? ann)) (private:bp-stat! 'not-annotation)]
      [(not (source-object? (annotation-source ann))) (private:bp-stat! 'no-source)]
      [(not (string=? path (source-file-descriptor-path (source-object-sfd (annotation-source ann)))))
        (private:bp-stat! 'path-mismatch)]
      [else (private:bp-stat! 'path-hit)]))
  (let ([here (private:copy-substitutions! virtual-node real-document path start end)])
    (+ here
      (apply +
        (map
          (lambda (child)
            (private:back-propagate! child real-document path start end))
          (index-node-children virtual-node))))))

(define (private:build-index-tree parent ann)
  ;;Build the virtual tree OURSELVES instead of init-index-node: that
  ;;builder is designed for reader-produced data (it filters non-annotation
  ;;children, synthesizes pair annotations, and dedups compounds by
  ;;identity -- measured: it silently dropped reconstructed subtrees that
  ;;violated those assumptions). Here every child is kept unconditionally
  ;;and every node lands exactly where the reconstruction put it.
  (let* ([src (annotation-source ann)]
      [node (make-index-node parent
              (source-object-bfp src) (source-object-efp src)
              ann #f '() '() '() '())]
      [expr (annotation-expression ann)])
    (when (pair? expr)
      (index-node-children-set! node
        (let loop ([es expr] [acc '()])
          (cond
            [(null? es) (reverse acc)]
            [(annotation? (car es))
              (loop (cdr es)
                (cons (private:build-index-tree node (car es)) acc))]
            ;;bare elements are literal data (e.g. a quote's datum): no
            ;;node, exactly like the reader's quoted content
            [else (loop (cdr es) acc)]))))
    node))

(define (private:count-symbol-at root symbol bias)
  (let ([count 0])
    (let walk ([n root])
      (when (and (index-node-symbol? n)
              (eq? symbol (index-node-expression n)))
        (let ([a (index-node-datum/annotations n)])
          (when (and (annotation? a) (source-object? (annotation-source a))
                  (= bias (source-object-bfp (annotation-source a))))
            (set! count (+ count 1)))))
      (for-each walk (index-node-children n)))
    count))

(define (take-some2 ls n)
  (let loop ([ls ls] [acc '()] [k 0])
    (if (or (null? ls) (= k n)) (reverse acc) (loop (cdr ls) (cons (car ls) acc) (+ k 1)))))

(define (private:node-head node)
  (let ([a (index-node-datum/annotations node)])
    (if (annotation? a)
      (let ([e (annotation-expression a)])
        (cond
          [(symbol? e) e]
          [(and (pair? e) (annotation? (car e)))
            (let ([h (annotation-expression (car e))])
              (and (symbol? h) h))]
          [else #f]))
      #f)))

(define (private:register-bindings! virtual-document node)
  ;;The expansion's LOCAL binders (v/w/a/b -- the pattern variables) get no
  ;;references from anywhere: the virtual tree has no identifier pass. But
  ;;USE positions resolve through find-available-references-for, which only
  ;;walks import lists -- without binder references the type chain dies at
  ;;the first use (measured: w-use resolved to nothing, a's chain stopped
  ;;one link short). This mirrors what the identifier side's lambda/let
  ;;rules do in real documents: register each binder as a reference ON THE
  ;;BINDING FORM's import list, so every use in the subtree resolves upward.
  (let ([head (private:node-head node)]
      [children (index-node-children node)])
    (cond
      [(and (memq head '(let let* letrec letrec*))
            (>= (length children) 2))
        (let ([bindings (cadr children)])
          (when (index-node? bindings)
            (for-each
              (lambda (pair)
                (let ([pcs (index-node-children pair)])
                  (when (and (pair? pcs)
                          (index-node-symbol? (car pcs)))
                    (let* ([binder (car pcs)]
                        [reference
                          (make-identifier-reference
                            (index-node-expression binder)
                            virtual-document binder binder
                            '() 'variable '() '() '())])
                      (index-node-references-import-in-this-node-set! node
                        (append (index-node-references-import-in-this-node node)
                          (list reference)))))))
              (index-node-children bindings))))]
      [(and (eq? head 'case-lambda) (pair? children))
        (for-each
          (lambda (clause)
            (let ([ccs (index-node-children clause)])
              (when (and (pair? ccs) (index-node? (car ccs)))
                (let ([formals (index-node-children (car ccs))])
                  (for-each
                    (lambda (formal)
                      (when (index-node-symbol? formal)
                        (let ([reference
                                (make-identifier-reference
                                  (index-node-expression formal)
                                  virtual-document formal formal
                                  '() 'parameter '() '() '())])
                          (index-node-references-import-in-this-node-set! node
                            (append (index-node-references-import-in-this-node node)
                              (list reference))))))
                    formals)))))
          (cdr children))])
    (for-each
      (lambda (child) (private:register-bindings! virtual-document child))
      children)))

(define (type:expand-and-infer run-generator root-library-node document call-node)
  ;;TYPE-side entry for the type router's generic macro arm: expand, run
  ;;the generator over the expansion, return the results to the call
  ;;site. Returns the number of substitution entries back-propagated, or
  ;;#f on any failure (silent no-analysis, per the IR route's semantics).
  (when (and (getenv "EXPAND_DEBUG") (not (hashtable-ref private:virtual-documents document #f)))
    (let* ([uri (document-uri document)]
        [len (string-length uri)]
        [from (max 0 (- len 30))])
      (format #t "[macro-expansion] enter at-bias=~a doc=~a~%"
        (index-node-start call-node) (substring uri from len))))
  ;;never re-expand inside a virtual expansion document: the arm fires
  ;;there too (expansions contain further macro calls), which would
  ;;recurse without bound
  (if (hashtable-ref private:virtual-documents document #f)
    #f
    (let ([expanded (ir-auto-route:expand-call-site root-library-node document call-node)])
    (and expanded
      (guard (e [else
                (when (getenv "EXPAND_DEBUG")
                  (format #t "[macro-expansion] FAIL: ~a~%"
                    (guard (c [#t (format "~a" e)])
                      (call-with-string-output-port
                        (lambda (p) (display-condition e p))))))
                #f])
        (let* ([path (uri->path (document-uri document))]
            ;;the virtual document INHERITS the caller's reference table:
            ;;every name the expansion uses was resolvable in the caller's
            ;;compile environment by construction, so the caller's
            ;;document-ordered-reference-list (populated by the identifier
            ;;pass with real library metadata -- type-expressions included)
            ;;is exactly the resolution context the virtual tree needs;
            ;;find-available-references-for falls into it at the parent-
            ;;chain ceiling. This replaces an earlier hand-seeding layer
            ;;(per-symbol constructed references) that had to fake the
            ;;metadata and broke in a dozen subtle ways.
            [virtual-document
              (make-document
                (string-append (document-uri document) "#macro-expansion")
                (document-text document)
                (document-ordered-reference-list document))]
            ;;parent under a virtual wrapper like the real document
            ;;pipeline: several rules reach (index-node-parent node)
            ;;unguarded, and the reference walk requires the ceiling
            ;;node's parent to be '()
            [virtual-wrapper (make-virtual-index-node)]
            [virtual-root (private:build-index-tree virtual-wrapper expanded)]
            [_ (document-index-node-list-set! virtual-document (list virtual-root))]

            ;;register BEFORE running: the generic arm fires inside the
            ;;expansion too (it contains further macro calls) and must
            ;;not re-expand there
            [_ (hashtable-set! private:virtual-documents virtual-document #t)]
            [_ (private:register-bindings! virtual-document virtual-root)]
            [_ (run-generator virtual-document)]
            [_ (when (getenv "EXPAND_DEBUG")
    (let ([typed 0])
      (let walk ([n virtual-root])
        (when (pair? (index-node-substitution-list n))
          (set! typed (+ typed 1)))
        (for-each walk (index-node-children n)))
      (format #t "[macro-expansion] typed nodes: ~a~%" typed)))]
            [start (index-node-start call-node)]
            [end (index-node-end call-node)]
            [copied (private:back-propagate! virtual-root document path start end)])
          (private:retain! document start end virtual-document)
          (when (getenv "EXPAND_DEBUG")
            (format #t "[macro-expansion] virtual diagnoses: ~a~%"
              (length (document-diagnoses virtual-document)))
            (for-each
              (lambda (d) (format #t "  vdiag: ~a @~a-~a~%" (list-ref d 3) (list-ref d 0) (list-ref d 1)))
              (take-some2 (document-diagnoses virtual-document) 5)))
          (when (getenv "EXPAND_DEBUG")
            (format #t "[macro-expansion] built=~a a@150=~a b@152=~a~%"
              (let count ([n virtual-root] [c 0])
                (fold-left (lambda (acc ch) (count ch acc))
                  (+ c 1) (index-node-children n)))
              (private:count-symbol-at virtual-root 'a 150)
              (private:count-symbol-at virtual-root 'b 152))
            (format #t "[macro-expansion] span=~a-~a back-propagated=~a stats=~a~%"
              start end copied
              (map (lambda (k) (cons k (hashtable-ref private:bp-stats k 0)))
                '(nodes not-annotation no-source path-mismatch path-hit))))
          copied))))))
)
