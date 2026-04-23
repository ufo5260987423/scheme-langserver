(library (scheme-langserver protocol apis file-change-notification)
  (export 
    did-create
    did-delete
    did-rename
    did-change-watched-files)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis dependency file-linkage)

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

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
(define (did-change-watched-files workspace params)
  (let* ([changes (vector->list (assq-ref params 'changes))]
      [root-file-node (workspace-file-node workspace)]
      [facet (workspace-facet workspace)]
      [top-env (workspace-top-environment workspace)]
      [body (lambda ()
              (for-each
                (lambda (change)
                  (let* ([uri (assq-ref change 'uri)]
                      [type (assq-ref change 'type)]
                      [path (uri->path uri)])
                    (when (facet path)
                      (case type
                        [1 (attach-new-file path root-file-node facet top-env)]
                        [2 (let ([file-node (walk-file root-file-node path)])
                             (when (file-node? file-node)
                               (let ([text (read-string path)])
                                 (when (string? text)
                                   (update-file-node-with-tail workspace file-node text)))))]
                        [3 (let ([file-node (walk-file root-file-node path)])
                             (when (file-node? file-node)
                               (did-delete workspace 
                                 (make-alist 'files (vector (make-alist 'uri uri))))))]))))
                changes))])
    (if (null? (workspace-mutex workspace))
      (body)
      (with-mutex (workspace-mutex workspace)
        (body)))))

;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didDeleteFiles
(define (did-delete workspace params)
  (let* ([files (vector->list (assq-ref params 'files))]
      [uris (map (lambda (file) (assq-ref file 'uri)) files)]
      [paths (map uri->path uris)]
      [linkage (workspace-file-linkage workspace)])
    (map 
      (lambda (file-node)
        (if (file-node? file-node)
          (begin
            (shrink-file-linkage! linkage (file-node-path file-node))
            (file-node-children-set!
              (file-node-parent file-node)
              (filter 
                (lambda (siblins)
                  (not (equal? file-node siblins)))
                (file-node-children (file-node-parent file-node)))))))
      (map 
        (lambda (path) (walk-file (workspace-file-node workspace) path))
        paths))))
)