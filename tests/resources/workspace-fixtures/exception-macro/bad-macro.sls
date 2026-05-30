(library (bad-macro)
  (export bad-macro)
  (import (chezscheme))

  (define-syntax bad-macro
    (lambda (x)
      (syntax-case x ()
        [(_) (error 'bad-macro "intentional expansion error for testing")]))))
