(library (scheme-langserver analysis identifier rules with-syntax)
  (export with-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util contain)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules syntax-case)
    (scheme-langserver analysis identifier rules let-syntax)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (with-syntax-process root-file-node root-library-node document index-node)
  (let-syntax-process root-file-node root-library-node document index-node))
)
