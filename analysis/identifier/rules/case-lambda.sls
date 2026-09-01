(library (scheme-langserver analysis identifier rules case-lambda)
  (export case-lambda-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier rules lambda)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; parameter 
(define (case-lambda-process root-file-node root-library-node document index-node)
  (match-index-node index-node 
    [(:_ clause-bodies **1)
      (map 
        (lambda (clause-body)
          (match-index-node clause-body
            [(((? index-node-symbol? parameters) **1) . body)
              (let ([pairs (collect-parameter-pairs (car (index-node-children clause-body)))])
                (check-duplicate-identifiers document pairs)
                (map 
                  (lambda (parameter)
                    (parameter-process index-node parameter (index-node-parent parameter) clause-body document))
                  parameters))]
            [((? index-node-symbol? parameter) . body)
              (parameter-process index-node parameter clause-body clause-body document)]
            [else '()]))
        clause-bodies)]
    [else '()]))
)
