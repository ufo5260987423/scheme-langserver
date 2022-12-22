(library (scheme-langserver protocol message)
  (export 
    response
    make-response
    response?
    response-id
    response-content
    response-error

    request
    make-request
    request?
    request-id
    request-params
    request-method

    read-message
    send-message
    send-result
    send-error
    send-notification

    make-server
    server?
    server-mutex
    server-shutdown?
    server-shutdown?-set!
    server-workspace
    server-workspace-set!
    server-thread-pool

    do-log

    success-response
    fail-response)
  (import 
    (chezscheme) 
    (scheme-langserver util json)
    (scheme-langserver util association)
    (scheme-langserver util io)
    (scheme-langserver protocol alist-access-object)
    (only (srfi :13 strings) string-index string-take string-drop ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type server
    (fields 
        (immutable input-port)
        (immutable output-port)
        (immutable log-port)
        (immutable thread-pool)
        ;;for output-port
        (immutable mutex)
        ; (immutable condition)
        (mutable workspace)
        (mutable shutdown?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type response
    (fields 
        (immutable id)
        (immutable content)
        (immutable error)))

(define-record-type request 
    (fields 
        (immutable id)
        (immutable method)
        (immutable params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (do-log message server-instance)
    (if (not (null? (server-log-port server-instance)))
        (begin 
            (put-string (server-log-port server-instance) message)
            (put-string (server-log-port server-instance) "\n")
            (flush-output-port (server-log-port server-instance)))))

(define (read-message server-instance)
    (let* ( 
            [header-hashtable (read-headers (server-input-port server-instance))]
            [json-content (read-content header-hashtable (server-input-port server-instance))])
        (do-log "read-message" server-instance)
        (do-log json-content server-instance)
        (parse-content json-content)))

;; header
;;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#headerPart
(define (read-headers port)
    (let loop (
            [line (read-to-CRNL port)]
            [header-hashtable (make-hashtable string-hash string=?)])
        (if (equal? line "")
            header-hashtable
            (let* ( [i (string-index line #\:)])
                (if i (hashtable-set! header-hashtable (string-take line i) (string-drop line (+ i 2))))
                (loop (read-to-CRNL port) header-hashtable)))))

(define (get-content-length header-hashtable)
    (string->number (hashtable-ref header-hashtable "Content-Length" string=?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-content header-hashtable port)
    (let ([utf8-transcoder (make-transcoder (utf-8-codec))]
            [content-length (get-content-length header-hashtable)])
        (bytevector->string (get-bytevector-n port content-length) utf8-transcoder)))

(define (parse-content json-string)
    (let ([content-alist (read-json json-string)])
        (make-request
            (assq-ref content-alist 'id)
            (assq-ref content-alist 'method)
            (assq-ref content-alist 'params))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (success-response id result-alist)
  (make-alist 'jsonrpc "2.0" 'id id 'result result-alist))

(define fail-response 
    (case-lambda 
        ([id code message] (make-alist 'jsonrpc "2.0" 'id id 'error (make-alist 'code code 'message message)))
        ([id code message data] (make-alist 'jsonrpc "2.0" 'id id 'error (make-alist 'code code 'message message 'data data)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define (send-notification server-instance method params)
  (send-message server-instance `((method . ,method) (params . ,params))))

(define send-error
    (case-lambda
        [(server-instance request-id error-id error-message) (send-error server-instance request-id error-id error-message #f)]
        [(server-instance request-id error-id error-message data) 
            (let ([error
                    `((code . ,error-id) (message . ,error-message) ,@(if (eq? data #f) '() `(data . ,data))) ])
                (send-message server-instance `((id . ,request-id) (error . ,error))))]))

(define (send-result server-instance request-id result)
  (send-message server-instance `((id . ,request-id) (result . ,result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#client_registerCapability
; Server must not register the same capability both statically through the initialize result and dynamically for the same document selector. If a server wants to support both static and dynamic registration it needs to check the client capability in the initialize request and only register the capability statically if the client doesn’t support dynamic registration for that capability.
(define (send-register-capability server-instance . registrations)
  (send-message server-instance `(;; FIXME: handle response
                   ok   (id . #f)
                      (method . "client/registerCapability")
                      (params . ((registrations . ,registrations))))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
; Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid in the scope of a resource.
(define (send-diagnostics server-instance uri diagnostics)
  (send-notification server-instance "textDocument/publishDiagnostics"
                    `((uri . ,uri)
                      (diagnostics . ,(map diagnostic->alist diagnostics)))))
)