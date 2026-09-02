(library (scheme-langserver analysis type substitutions rules let*)
  (export let*-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis type substitutions rules let))

(define (let*-process document index-node)
  (match-index-node index-node
    [(:_ ((left value) ...) :_ ... return)
      (let:emit-substitutions! index-node return left value)]
    [else '()]))
)
