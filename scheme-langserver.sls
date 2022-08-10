(library (scheme-langserver)
  (export 
    server-document-hashtable
    server-mutex
    server-output-port
    server-input-port
    init-server)
  (import 
    (chezscheme) 
    (ufo-thread-pool) 
    (ufo-match) 
    (srfi :13 strings)
    (scheme-langserver protocol error-code) 
    (scheme-langserver protocol message)
    (scheme-langserver util association))

(define-record-type server
    (fields 
        (immutable input-port)
        (immutable output-port)
        (immutable thread-pool)
        ;;for output-port
        (immutable mutex)
        ; (immutable condition)
        (mutable document-hashtable)
        (mutable shutdown?))
        (mutable index)))

; (define-record-type state
;     (fileds 
;       (immutable root-uri)
;       (immutable documents)
;       (immutable shutdown?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes a request. This procedure should always return a response
(define (process-request server request)
  (let([method (request-method request)]
        [id (request-id request)]
        [params (request-params request)])
    (match method
      ["initialize"
       (initialize server id params)]
      ["shutdown"
       (shutdown server id)]
      ["textDocument/hover"
       (text-document/hover id params)]
      ["textDocument/completion"
       (text-document/completion id params)]
      ["textDocument/signatureHelp"
       (text-document/signatureHelp id params)]
      ["textDocument/definition"
       (text-document/definition id params)]
      ["textDocument/documentHighlight"
       (text-document/document-highlight id params)]
      ["textDocument/references"
       (text-document/references id params)]
      ["textDocument/documentSymbol"
       (text-document/document-symbol id params)]
      ; ["textDocument/rename"
      ;  (text-document/rename id params)]
      ; ["textDocument/prepareRename"
      ;  (text-document/prepareRename id params)]
      ; ["textDocument/formatting"
      ;  (text-document/formatting! id params)]
      ; ["textDocument/rangeFormatting"
      ;  (text-document/range-formatting! id params)]
      ; ["textDocument/onTypeFormatting"
      ;  (text-document/on-type-formatting! id params)]
      [_
       (pretty-print (string-append "invalid request for method " method " \n"))
       (raise method-not-found)])))

    ; connection.onDidChangeConfiguration(server.didChangeConfiguration.bind(server));

    ; connection.onDidSaveTextDocument(server.didSaveTextDocument.bind(server));
    ; connection.onDidCloseTextDocument(server.didCloseTextDocument.bind(server));
    ; connection.onDidChangeTextDocument(server.didChangeTextDocument.bind(server));

    ; connection.onCodeAction(server.codeAction.bind(server));
    ; connection.onCompletionResolve(server.completionResolve.bind(server));

    ; connection.onImplementation(server.implementation.bind(server));
    ; connection.onTypeDefinition(server.typeDefinition.bind(server));

    ; connection.onDocumentFormatting(server.documentFormatting.bind(server));
    ; connection.onDocumentSymbol(server.documentSymbol.bind(server));
    ; connection.onExecuteCommand(server.executeCommand.bind(server));
    ; connection.onRenameRequest(server.rename.bind(server));
    ; connection.onSignatureHelp(server.signatureHelp.bind(server));
    ; connection.onWorkspaceSymbol(server.workspaceSymbol.bind(server));
    ; connection.onFoldingRanges(server.foldingRanges.bind(server));

    ; // proposed `textDocument/calls` request
    ; connection.onRequest(lspcalls.CallsRequest.type, server.calls.bind(server));

    ; connection.onRequest(lspinlayHints.type, server.inlayHints.bind(server));

    ; connection.onRequest(lsp.SemanticTokensRequest.type, server.semanticTokensFull.bind(server));
    ; connection.onRequest(lsp.SemanticTokensRangeRequest.type, server.semanticTokensRange.bind(server));

;; not reply client!
(define (process-notification server request)
  (let([method (request-method request)]
        [id (request-id request)]
        [params (request-params request)])
    (match method
      ["exit"
        (with-mutex (server-mutex server)
         (exit  
          (if (server-shutdown? server) 1 0)))
        ]
      ["textDocument/didOpen"
        (text-document/did-open! server params)]
      ["textDocument/didClose"
        (text-document/did-close! params)]
      ["textDocument/didChange"
        (text-document/did-change! params)]
      [_ (void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initialize server id params)
  (let* (
        [root-path (uri->path (assq-ref 'rootUri params))]
        [client-capabilities (assq-ref 'capabilities params)]
        [sync-options (make-alist 
              'openClose #t 
              ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
              ;; Incremental=2
              'change 2
              'willSave #f 
              'willSaveWaitUntil #f)]
        [server-capabilities (make-alist 
              'textDocumentSync sync-options
              'hoverProvider #t
              'definitionProvider #t
              'referencesProvider #t
              'completionProvider (make-alist 'triggerCharacters (list "("))
              ; 'signatureHelpProvider (make-alist 'triggerCharacters (list " " ")" "]"))
              'implementationProvider #t
              'renameProvider #t
              ; 'documentHighlightProvider #t
              ; 'documentSymbolProvider #t
              ; 'documentLinkProvider #t
              ; 'documentFormattingProvider #t
              ; 'documentRangeFormattingProvider #t
              ; 'documentOnTypeFormattingProvider (make-alist 'firstTriggerCharacter ")" 'moreTriggerCharacter (list "\n" "]"))
              ; 'foldingRangeProvider #t
              ; 'workspace workspace-configuration
              )]
        ; [workspace-configuration (make-alist 'workspaceFolders (make-alist))]
        )
    (with-mutex (server-mutex server)
      (server-index-set! server (init-index root-path)))
      ;;todo start server 
    (success-response id (make-alist 'capabilities server-capabilities))))

(define (shutdown server id)
;;todo: kill server
  (with-mutex (server-mutex server)
    (server-shutdown-set! server #t)
    (thread-pool-stop (server-thread-pool server))
    (success-response id '())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (process-message server message)
  (cond 
  ;;notification do not require response
    ([(null? (request-id message))] (process-notification server message))
    (else (send-message server (process-request server message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init-server
    (case-lambda
        [() (init-server (current-input-port) (current-output-port))]
        [(input-port output-port) 
          (let ([server (make-server 
                    input-port 
                    output-port 
                    (if (threaded?) (init-thread-pool 4 #t) '()) 
                    (if (threaded?) (make-mutex) '()) 
                    (if (threaded?) (make-condition) '()) 
                    (make-eq-hashtable) 
                    #f
                    '())])
            (let loop ([message (read-message server)])
            ;;log
              (pretty-print message)
              (if (null? (server-thread-pool))
                (process-message server message)
                (thread-pool-add-job (lambda() (process-message server message))))
              (loop (read-message server)))
            (newline)
            (newline)
            (display "bye")
            (newline))]))
)
