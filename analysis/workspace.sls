(library (scheme-langserver analysis workspace)
  (export init-workspace)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver analyse index)
    (scheme-langserver analyse virtual-file-system file-node))

(define-record-type workspace
  (fields
    (immutable file-node)
    ))
)