(library (scheme-langserver analysis syntax expanding-rule-generator)
  (export 
    generate)
  (import 
    (chezscheme)
    (ufo-match)
    (ufo-try)

    (scheme-langserver analysis syntax util)
    (scheme-langserver analysis syntax rules define-syntax)
    (scheme-langserver analysis syntax rules let-syntax)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis tokenizer)

    (scheme-langserver util contain)
    (scheme-langserver util path)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (generate target-identifier document index-node)
  (let* ([top-identifier (root-ancestor target-identifier)]
      [top-symbol (identifier-reference-identifier top-identifier)])
    (case top-symbol
      [define-syntax (define-syntax-process document index-node syntax-stepper)]
      [let-syntax (let-syntax-process document index-node syntax-stepper)]
      [letrec-syntax (let-syntax-process document index-node syntax-stepper)]
      [else '()])))
)