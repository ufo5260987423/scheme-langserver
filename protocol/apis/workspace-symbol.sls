(library (scheme-langserver protocol apis workspace-symbol)
  (export 
    workspace-symbol)
  (import 
    (chezscheme)

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util path)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_symbol
(define (workspace-symbol workspace params)
  (let* ([query (assq-ref params 'query)]
      [root-file-node (workspace-file-node workspace)]
      [all-symbols (private-collect-all-symbols root-file-node)]
      [filtered-symbols 
        (if (or (null? query) (equal? query ""))
          all-symbols
          (filter 
            (lambda (symbol-info)
              (private-matches-query? query (assq-ref symbol-info 'name)))
            all-symbols))])
    (list->vector filtered-symbols)))

(define (private-collect-all-symbols file-node)
  (if (file-node-folder? file-node)
    (apply append (map private-collect-all-symbols (file-node-children file-node)))
    (private-collect-symbols-from-file file-node)))

(define (private-collect-symbols-from-file file-node)
  (let ([document (file-node-document file-node)])
    (if (null? document)
      '()
      (let ([references (document-ordered-reference-list document)])
        (map 
          (lambda (ref)
            (private-identifier-reference->symbol-information ref file-node))
          references)))))

(define (private-identifier-reference->symbol-information identifier-reference file-node)
  (let* ([document (identifier-reference-document identifier-reference)]
      [index-node (identifier-reference-index-node identifier-reference)]
      [name (symbol->string (identifier-reference-identifier identifier-reference))]
      [uri (path->uri (file-node-path file-node))]
      [range 
        (if (null? index-node)
          (make-range (make-position 0 0) (make-position 0 0))
          (let ([start (apply make-position (document+bias->position-list document (index-node-start index-node)))]
              [end (apply make-position (document+bias->position-list document (index-node-end index-node)))])
            (make-range start end)))])
    (make-alist 
      'name name
      'kind 13
      'location (location->alist (make-location uri range)))))

(define (private-matches-query? query name)
  (let ([query-lower (string-downcase query)]
      [name-lower (string-downcase name)])
    (private-string-contains? name-lower query-lower)))

(define (private-string-contains? str sub)
  (let ([str-len (string-length str)]
      [sub-len (string-length sub)])
    (if (zero? sub-len)
      #t
      (let loop ([i 0])
        (if (> (+ i sub-len) str-len)
          #f
          (or (string=? sub (substring str i (+ i sub-len)))
            (loop (+ i 1))))))))
)
