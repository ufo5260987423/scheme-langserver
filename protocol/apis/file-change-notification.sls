(library (scheme-langserver protocol apis file-change-notification)
  (export 
    did-create
    did-delete
    did-rename)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util path) 
    (ufo-try) 
    (scheme-langserver util text) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis package-manager txt-filter)

    (only (srfi :13 strings) string-replace))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didCreateFiles
(define (did-create workspace params)
  (let* ([files (vector->list (assq-ref params 'files))]
      [uris (map (lambda (file) (assq-ref file 'uri)) files)]
      [paths (map uri->path uris)]
      [facet (workspace-facet workspace)]
      [root-file-node (workspace-file-node workspace)])
    (map 
      (lambda (file-node)
        (refresh-workspace-for workspace file-node))
      (map 
        (lambda (path) (attach-new-file path root-file-node facet))
        paths))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didRenameFiles
(define (did-rename workspace params)
  (map 
    (lambda (file)
      (let* ([old (assq-ref file 'oldUri)]
          [old-path (uri->path old)]
          [old-file-node (walk-file (workspace-file-node workspace) old-path)]
          [new (assq-ref file 'newUri)]
          [new-path (uri->path new)]
          [root-file-node (workspace-file-node workspace)]
          [facet (workspace-facet workspace)]
          [new-file-node (attach-new-file new-path root-file-node facet)])
        (file-node-document-set! new-file-node (file-node-document old-file-node))))
    (vector->list (assq-ref params 'files))))

;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didDeleteFiles
(define (did-delete workspace params)
  (let* ([files (vector->list (assq-ref params 'files))]
      [uris (map (lambda (file) (assq-ref file 'uri)) files)]
      [paths (map uri->path uris)])
    (map 
      (lambda (file-node)
        (if (file-node? file-node)
          (file-node-children-set!
            (file-node-parent file-node)
            (filter 
              (lambda (siblins)
                (not (equal? file-node siblins)))
              (file-node-children (file-node-parent file-node))))))
      (map 
        (lambda (path) (walk-file (workspace-file-node workspace) path))
        paths))))
)