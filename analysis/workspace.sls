(library (scheme-langserver analysis workspace)
  (export init-workspace)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver analysis index)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis virtual-source-file-system)
    )

(define-record-type source-linkage-node 
  (immutable parent)
  (immutable children)
)

(define-record-type workspace
  (fields
    (immutable file-node)
    (mutable source-node)))

(define (init-workspace path)
  (let* ([file-node (init-virtual-file-system path '() folder-or-scheme-file?)]
        [workspace-instance (make-workspace file-node (init-virtual-source-file-system file-node))])
    workspace-instance
  ))
)