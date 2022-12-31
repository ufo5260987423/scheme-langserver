(library (scheme-langserver protocol response)
  (export 
    response
    make-response
    response?
    response-id
    response-content
    response-error

    send-message

    success-response
    fail-response)
  (import 
    (chezscheme) 
    (scheme-langserver util json)
    (scheme-langserver util association)
    (scheme-langserver util io)
    (scheme-langserver protocol server)
    (scheme-langserver protocol alist-access-object)
    (only (srfi :13 strings) string-index string-take string-drop ))

(define-record-type response
    (fields 
        (immutable id)
        (immutable content)
        (immutable error)))

(define (success-response id result-alist)
  (make-alist 'jsonrpc "2.0" 'id id 'result result-alist))

(define fail-response 
    (case-lambda 
        ([id code message] (make-alist 'jsonrpc "2.0" 'id id 'error (make-alist 'code code 'message message)))
        ([id code message data] (make-alist 'jsonrpc "2.0" 'id id 'error (make-alist 'code code 'message message 'data data)))))

(define (send-message server-instance response-alist)
    (do-log "send-message" server-instance)
    (let* (
            [body-json (generate-json response-alist)]
            [body (string->utf8 body-json)]
            [header (string->utf8 (string-append 
                        "Content-Length: " (number->string (bytevector-length body)) "\r\n"
                        "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n"))]
            [port (server-output-port server-instance)])
        (do-log body-json server-instance)
        (if (null? (server-mutex server-instance))
            (begin 
                (put-bytevector port header)
                (put-bytevector port body )
                ; (write-string header port)
                ; (write-string body port)
                (flush-output-port port))
            (with-mutex (server-mutex server-instance)
                ; (write-string header port)
                ; (write-string body port)
                (put-bytevector port header)
                (put-bytevector port body )
                (flush-output-port port)))))
)