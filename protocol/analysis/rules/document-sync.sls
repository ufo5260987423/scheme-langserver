(library (scheme-langserver protocol analysis rules document-sync)
  (export 
    process-document-sync)
  (import 
    (chezscheme)
    (slib queue)
    
    (scheme-langserver util path)

    (scheme-langserver protocol request)
    (scheme-langserver protocol analysis util)
    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association))

(define (process-document-sync head-request queue)
  (let* ([head-method (request-method head-request)]
      [head-param (request-params head-request)])
    (if (equal? head-method "textDocument/didChange")
      (let* ([head-versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref head-param 'textDocument))]
          [head-path (uri->path (versioned-text-document-identifier-uri head-versioned-text-document-identifier))]
          [predicator 
            (lambda (request)
              (let* ([method (request-method request)]
                  [param (request-params request)]
                  [versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref param 'textDocument))]
                  [path (uri->path (versioned-text-document-identifier-uri versioned-text-document-identifier))])
                (and (equal? method "textDocument/didChange") (equal? head-path path))))]
          [result (scan-queue&pick-out queue predicator)])
        (if (null? result)
          head-request
          (fold-left
            private-process:changes->change
            head-request
            result)))
      head-request)))

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