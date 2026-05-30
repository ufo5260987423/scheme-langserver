(library (consumer)
  (export foo)
  (import (chezscheme)
    (bad-macro))

  (bad-macro)
  
  (define foo 1))
