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

;; Processes a request. This procedure should always return a response
(define (process-request server-instance request)
  (let([method (request-method request)]
        [id (request-id request)]
        [params (request-params request)])
    (match method
      ["initialize"
       (initialize server-instance id params)]
      ["shutdown"
       (shutdown server-instance id)]
      ;; text document 
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
(define (process-notification server-instance request)
  (let([method (request-method request)]
        [id (request-id request)]
        [params (request-params request)])
    (match method
      ["exit"
        (if (null? (server-mutex server-instance))
          (exit  (if (server-shutdown? server-instance) 1 0))
          (with-mutex (server-mutex server-instance)
            (exit  (if (server-shutdown? server-instance) 1 0))))]
      ; ["textDocument/didOpen"
      ;   (text-document/did-open! server params)]
      ; ["textDocument/didClose"
      ;   (text-document/did-close! params)]
      ["textDocument/didChange"
        (text-document/did-change! params)]
      ["textDocument/didSave"
       (text-document/didSave id params)]
      ["textDocument/rename"
       (text-document/rename id params)]
      [_ (void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initialize server-instance id params)
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
              ; 'workspaceSymbol #t
              ; 'typeDefinitionProvider #t
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
              ; 'colorProvider #t
              ; 'workspace workspace-configuration
              )]
        ; [workspace-configuration (make-alist 'workspaceFolders (make-alist))]
        )
    (with-mutex (server-mutex server-instance)
      (server-index-set! server-instance (init-index root-path)))
      ;;todo start server 
    (success-response id (make-alist 'capabilities server-capabilities))))

(define (shutdown server-instance id)
;;todo: kill server
  (with-mutex (server-mutex server-instance)
    (server-shutdown-set! server-instance #t)
    (thread-pool-stop (server-thread-pool server-instance))
    (success-response id '())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (process-message server-instance message)
  (cond 
  ;;notification do not require response
    ([(null? (request-id message))] (process-notification server-instance message))
    (else (send-message server-instance (process-request server-instance message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init-server
    (case-lambda
        [() (init-server (current-input-port) (current-output-port))]
        [(input-port output-port) 
          (let ([server-instance 
                  (if (threaded?)
                    (make-server input-port output-port (init-thread-pool 4 #t) (make-mutex) (make-condition) (make-eq-hashtable) #f '())
                    (make-server input-port output-port '() '() '() (make-eq-hashtable) #f '())) ])
            (let loop ([message (read-message server-instance)])
            ;;log
              (pretty-print message)
              (if (null? (server-thread-pool server-instance))
                (process-message server-instance message)
                (thread-pool-add-job (lambda() (process-message server-instance message))))
              (loop (read-message server-instance)))
            (newline)
            (newline)
            (display "bye")
            (newline))]))
)
