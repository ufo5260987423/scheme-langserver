(library (scheme-langserver protocol error-code)
  (export 
    parse-error
    invalid-request
    method-not-found
    invalid-params
    internal-error
    server-error-start
    server-error-end
    server-not-initialized
    unknown-error-code)
  (import (rnrs))

(define invalid-request -32600)
(define method-not-found -32601)
(define invalid-params -32602)
(define internal-error -32603)
(define server-error-start -32099)
(define server-error-end -32000)
(define server-not-initialized -32002)
(define unknown-error-code -32001)

;; Defined by JSON RPC
(define parse-error -32700)

;; Defined by LSP protocol
(define request-cancelled -32800)

)