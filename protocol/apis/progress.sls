(library (scheme-langserver protocol apis progress)
  (export request->progress)
  (import 
    (chezscheme) 
    (scheme-langserver protocol request)
    (scheme-langserver util association))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workDoneProgress
(define (request->progress kind request)
  (make-alist 'token (request-id request) 'value (make-alist 'kind kind 'title (request-params request))))

)
