(library (scheme-langserver protocol request)
  (export 
    request
    make-request
    request?
    request-id
    request-params
    request-method

    read-message)
  (import 
    (chezscheme) 
    (scheme-langserver util json)
    (scheme-langserver util association)
    (scheme-langserver util io)
    (scheme-langserver protocol server)
    (scheme-langserver protocol alist-access-object)
    (only (srfi :13 strings) string-index string-take string-drop ))

(define-record-type request 
    (fields 
        (immutable id)
        (immutable method)
        (immutable params)))

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
)