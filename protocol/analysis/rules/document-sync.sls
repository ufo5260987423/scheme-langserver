(library (scheme-langserver protocol analysis rules document-sync)
  (export 
    process-document-sync)
  (import 
    (ufo-match) 

    (chezscheme)
    (slib queue)
    
    (scheme-langserver util path)

    (scheme-langserver protocol request)
    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association))

(define (process-document-sync head-request queue)
  (let* ([next-request 
        (if (queue-empty? queue)
          '()
          (queue-front queue))]
      [head-method (request-method head-request)]
      [head-param (request-params head-request)]
      [head-versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref head-param 'textDocument))]
      [head-path (uri->path (versioned-text-document-identifier-uri head-versioned-text-document-identifier))])
    (if (null? next-request)
      head-request
      (let* ([next-method (request-method next-request)]
          [next-param (request-params next-request)]
          [next-versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref next-param 'textDocument))]
          [next-path (uri->path (versioned-text-document-identifier-uri next-versioned-text-document-identifier))])
        (match `(,head-method ,next-method)
          [("textDocument/didChange" "textDocument/didChange")
            (if (equal? next-path head-path)
              (private-process:changes->change head-request next-request)
              head-request)]
          [else head-request])))))

(define (private-process:changes->change head-request next-request)
  (let* ([head-param (request-params head-request)]
      [next-param (request-params next-request)]
      [head-changes (vector->list (assq-ref head-param 'contentChanges))]
      [next-changes (vector->list (assq-ref next-param 'contentChanges))]
      [sum-changes (list->vector (append head-changes next-changes))])
    (make-request 
      (request-id head-request)
      (request-method head-request)
      (make-alist 'textDocument (assq-ref head-param 'textDocument) 'contentChanges sum-changes))))
)