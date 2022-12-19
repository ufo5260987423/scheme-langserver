(library (scheme-langserver)
  (export 
    init-server)
  (import 
    (chezscheme) 
    (ufo-thread-pool) 
    (ufo-thread-pool util try) 
    (ufo-match) 

    (scheme-langserver analysis workspace)

    (scheme-langserver protocol error-code) 
    (scheme-langserver protocol message)

    (scheme-langserver protocol apis references)
    (scheme-langserver protocol apis completion)
    (scheme-langserver protocol apis hover)
    (scheme-langserver protocol apis definition)
    (scheme-langserver protocol apis document-sync)
    (scheme-langserver protocol apis document-symbol)

    (scheme-langserver util association)
    (scheme-langserver util path))

;; Processes a request. This procedure should always return a response
(define (process-request server-instance request)
  (let* ([method (request-method request)]
        [id (request-id request)]
        [params (request-params request)]
        [workspace (server-workspace server-instance)])
    (if 
      (and 
        (server-shutdown? server-instance)
        (not (equal? "initialize" method))
        (not (equal? "exit" method)))
      (send-message server-instance (fail-response id server-not-initialized "not initialized"))
      (match method
        ["initialize" (send-message server-instance (initialize server-instance id params))] 
        ["initialized" '()] 
        ["exit" '()] 
        ["shutdown" (send-message server-instance (shutdown server-instance id))]

        ["textDocument/didOpen" 
          (with-workspace-write workspace 
            (try
              (did-open workspace params)
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]
        ["textDocument/didClose" 
          (with-workspace-write workspace 
            (try
              (did-close workspace params)
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]
        ["textDocument/didChange" 
          (with-workspace-write workspace 
            (try
              (did-change workspace params)
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]

        ["textDocument/hover" 
          (with-workspace-read workspace
            (try
              (send-message server-instance (success-response id (hover workspace params)))
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]
        ["textDocument/completion" 
          (with-workspace-read workspace
            (try
              (send-message server-instance (success-response id (completion workspace params)))
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]
        ["textDocument/references" 
          (with-workspace-read workspace
            (try
              (send-message server-instance (success-response id (find-references workspace params)))
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]
          ; ["textDocument/signatureHelp"
          ;  (text-document/signatureHelp id params)]
        ["textDocument/definition" 
          (with-workspace-read workspace
            (try
              (send-message server-instance (success-response id (definition workspace params)))
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]
          ; ["textDocument/documentHighlight"
          ;  (text-document/document-highlight id params)]
        ["textDocument/documentSymbol" 
          (with-workspace-read workspace
            (try
              (send-message server-instance (success-response id (document-symbol workspace params)))
              (except c
                [else (send-message server-instance (fail-response id unknown-error-code method))])))]
          ; ["textDocument/prepareRename"
          ;  (text-document/prepareRename id params)]
          ; ["textDocument/formatting"
          ;  (text-document/formatting! id params)]
          ; ["textDocument/rangeFormatting"
          ;  (text-document/range-formatting! id params)]
          ; ["textDocument/onTypeFormatting"
          ;  (text-document/on-type-formatting! id params)]
          ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didChangeWatchedFilesClientCapabilities
          ; ["workspace/didChangeWatchedFiles"
          ;  (??? id params)]
        [_ (send-message server-instance (fail-response id method-not-found (string-append "invalid request for method " method " \n")))]))))
  ; public static final string text_document_formatting = "textdocument/formatting";
	; public static final string text_document_range_formatting = "textdocument/rangeformatting";
	; public static final string text_document_on_type_formatting = "textdocument/ontypeformatting";
	; public static final string text_document_code_lens = "textdocument/codelens";
	; public static final string text_document_signature_help = "textdocument/signaturehelp";
	; public static final string text_document_rename = "textdocument/rename";
	; public static final string workspace_execute_command = "workspace/executecommand";
	; public static final string workspace_symbol = "workspace/symbol";
	; public static final string workspace_watched_files = "workspace/didchangewatchedfiles";
	; public static final string document_symbol = "textdocument/documentsymbol";
	; public static final string code_action = "textdocument/codeaction";
	; public static final string typedefinition = "textdocument/typedefinition";
	; public static final string document_highlight = "textdocument/documenthighlight";
	; public static final string foldingrange = "textdocument/foldingrange";
	; public static final string workspace_change_folders = "workspace/didchangeworkspacefolders";
	; public static final string implementation = "textdocument/implementation";
	; public static final string selection_range = "textdocument/selectionrange";

;; not reply client!
; (define (process-notification server-instance request)
;   (let([method (request-method request)]
;         [id (request-id request)]
;         [params (request-params request)])
;     (match method
;       ["exit"
;         (if (null? (server-mutex server-instance))
;           (exit  (if (server-shutdown? server-instance) 1 0))
;           (with-mutex (server-mutex server-instance)
;             (exit  (if (server-shutdown? server-instance) 1 0))))]
;       ; ["textDocument/didSave"
;       ;  (text-document/didSave id params)]
;       ; ["textDocument/rename"
;       ;  (text-document/rename id params)]
;       [_ (void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initialize server-instance id params)
  (let* (
        [root-path (uri->path (assq-ref params 'rootUri))]
        [client-capabilities (assq-ref params 'capabilities)]
        [textDocument (assq-ref params 'textDocument)]
        ; [renameProvider 
        ;   (if (assq-ref (assq-ref (assq-ref params 'textDocumet) 'rename) 'prepareSupport)
        ;     (make-alist 'prepareProvider #t)
        ;     #t)]
        [workspace-configuration-body (make-alist 'workspaceFolders (make-alist 'changeNotifications #t 'supported #t))]

        [text-document-body (make-alist 
              'openClose #t 
              ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
              ;; Incremental=2
              'change 2)]
        [server-capabilities (make-alist 
              'textDocumentSync text-document-body
              'hoverProvider #t
              'definitionProvider #t
              'referencesProvider #t
              ; 'workspaceSymbol #t
              ; 'typeDefinitionProvider #t
              ; 'selectionRangeProvider #t
              ; 'callHierarchyProvider #t
              'completionProvider (make-alist 'triggerCharacters (vector "("))
              ; 'signatureHelpProvider (make-alist 'triggerCharacters (vector " " ")" "]"))
              ; 'implementationProvider #t
              ; 'renameProvider renameProvider
              ; 'codeActionProvider #t
              ; 'documentHighlightProvider #t
              'documentSymbolProvider #t
              ; 'documentLinkProvider #t
              ; 'documentFormattingProvider #t
              ; 'documentRangeFormattingProvider #t
              ; 'documentOnTypeFormattingProvider (make-alist 'firstTriggerCharacter ")" 'moreTriggerCharacter (vector "\n" "]"))
              ; 'codeLensProvider #t
              ; 'foldingRangeProvider #t
              ; 'colorProvider #t
              ; 'workspace workspace-configuration
              )]
              )
    (if (null? (server-mutex server-instance))
      (server-workspace-set! server-instance (init-workspace root-path))
      (with-mutex (server-mutex server-instance) 
        (if (null? (server-workspace server-instance))
          (server-workspace-set! server-instance (init-workspace root-path #t))
          (fail-response id server-error-start "server has been initialized"))))
    (success-response id (make-alist 'capabilities server-capabilities))))

(define (shutdown server-instance id)
;;todo: kill server
  (if (null? (server-mutex server-instance))
    (server-shutdown?-set! server-instance #t)
    (begin
      (thread-pool-stop! (server-thread-pool server-instance))
      (with-mutex (server-mutex server-instance)
        (server-shutdown?-set! server-instance #t))))
    (success-response id '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init-server
    (case-lambda
        [() (init-server (standard-input-port) (standard-output-port) '() #f)]
        [(log-path) (init-server (standard-input-port) (standard-output-port) (open-file-output-port log-path (file-options replace) 'block (make-transcoder (utf-8-codec))) #f)]
        [(input-port output-port log-port enable-multi-thread?) 
          (let ([server-instance 
                  (if enable-multi-thread?
                    (if threaded?
                      (make-server input-port output-port log-port (init-thread-pool 4 #t) (make-mutex) '() #f)
                      (make-server input-port output-port log-port '() '() '() #f)) 
                    (make-server input-port output-port log-port '() '() '() #f))])
            (try
              (let loop ([message (read-message server-instance)])
                (if (null? (server-thread-pool server-instance))
                  (process-request server-instance message)
                  (thread-pool-add-job (server-thread-pool server-instance) (lambda() (process-request server-instance message))))
                (loop (read-message server-instance)))
              (except c 
                [else 
                  (pretty-print `(format ,(condition-message c) ,@(condition-irritants c)))
                  (do-log (string-append "error: " (eval `(format ,(condition-message c) ,@(condition-irritants c)))) server-instance)
                  (shutdown server-instance -1)]))
            (newline)
            (display "bye"))]))
)
