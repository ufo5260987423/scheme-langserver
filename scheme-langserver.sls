(library (scheme-langserver)
  (export 
    init-server
    process-request)
  (import 
    (chezscheme) 
    (ufo-thread-pool) 
    (ufo-match) 

    (scheme-langserver analysis workspace)

    (scheme-langserver protocol error-code) 
    (scheme-langserver protocol message)
    (scheme-langserver util association)
    (scheme-langserver util path))

;; Processes a request. This procedure should always return a response
(define (process-request server-instance request)
  (do-log "process-request" server-instance)
  (do-log (request-method request) server-instance)
  (let* ([method (request-method request)]
        [id (request-id request)]
        [params (request-params request)])
    (match method
      ["initialize" (send-message server-instance (initialize server-instance id params))] 
      ["shutdown" (send-message server-instance (shutdown server-instance id))]
          ;; text document 
          ; ["textDocument/hover"
          ;  (text-document/hover id params)]
          ; ["textDocument/completion"
          ;  (text-document/completion id params)]
          ; ["textDocument/signatureHelp"
          ;  (text-document/signatureHelp id params)]
          ; ["textDocument/definition"
          ;  (text-document/definition id params)]
          ; ["textDocument/documentHighlight"
          ;  (text-document/document-highlight id params)]
          ; ["textDocument/references"
          ;  (text-document/references id params)]
          ; ["textDocument/documentSymbol"
          ;  (text-document/document-symbol id params)]
          ; ["textDocument/prepareRename"
          ;  (text-document/prepareRename id params)]
          ; ["textDocument/formatting"
          ;  (text-document/formatting! id params)]
          ; ["textDocument/rangeFormatting"
          ;  (text-document/range-formatting! id params)]
          ; ["textDocument/onTypeFormatting"
          ;  (text-document/on-type-formatting! id params)]
      [_ (fail-response id method-not-found (string-append "invalid request for method " method " \n"))])))
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
	; public static final string completion = "textdocument/completion";
	; public static final string code_action = "textdocument/codeaction";
	; public static final string definition = "textdocument/definition";
	; public static final string typedefinition = "textdocument/typedefinition";
	; public static final string references = "textdocument/references";
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
;       ; ["textDocument/didOpen"
;       ;   (text-document/did-open! server params)]
;       ; ["textDocument/didClose"
;       ;   (text-document/did-close! params)]
;       ; ["textDocument/didChange"
;       ;   (text-document/did-change! params)]
;       ; ["textDocument/didSave"
;       ;  (text-document/didSave id params)]
;       ; ["textDocument/rename"
;       ;  (text-document/rename id params)]
;       [_ (void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initialize server-instance id params)
  (let* (
        [root-path (uri->path (assq-ref 'rootUri params))]
        [client-capabilities (assq-ref 'capabilities params)]
        ; [renameProvider 
        ;   (if (assq-ref 'prepareSupport (assq-ref 'rename (assq-ref 'textDocumet params)))
        ;     (make-alist 'prepareProvider #t)
        ;     #t)]
        [sync-options (make-alist 
              'openClose #t 
              ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
              ;; Incremental=2
              'change 2
              'willSave #t 
              'willSaveWaitUntil #t)]
        [workspace-configuration (make-alist 'workspaceFolders (make-alist 'changeNotifications #t 'supported #t))]
        ; [server-capabilities (make-alist 
        ;       ; 'textDocumentSync sync-options
        ;       ; 'hoverProvider #t
        ;       'definitionProvider #t
        ;       'referencesProvider #t
        ;       ; 'workspaceSymbol #t
        ;       ; 'typeDefinitionProvider #t
        ;       ; 'selectionRangeProvider #t
        ;       ; 'callHierarchyProvider #t
        ;       ; 'completionProvider (make-alist 'triggerCharacters (list "("))
        ;       ; 'signatureHelpProvider (make-alist 'triggerCharacters (list " " ")" "]"))
        ;       ; 'implementationProvider #t
        ;       ; 'renameProvider renameProvider
        ;       ; 'codeActionProvider #t
        ;       ; 'documentHighlightProvider #t
        ;       ; 'documentSymbolProvider #t
        ;       ; 'documentLinkProvider #t
        ;       ; 'documentFormattingProvider #t
        ;       ; 'documentRangeFormattingProvider #t
        ;       ; 'documentOnTypeFormattingProvider (make-alist 'firstTriggerCharacter ")" 'moreTriggerCharacter (list "\n" "]"))
        ;       ; 'codeLensProvider #t
        ;       ; 'foldingRangeProvider #t
        ;       ; 'colorProvider #t
        ;       ; 'workspace workspace-configuration
        ;       )]
              )
    (do-log "init-workspace" server-instance) 
    ; (if (null? (server-mutex server-instance))
    ;   (server-workspace-set! server-instance (init-workspace root-path))
    ;   (with-mutex (server-mutex server-instance) 
    ;     (server-workspace-set! server-instance (init-workspace root-path))))
    ;   ;;todo start server 
    ; (success-response id (make-alist 'capabilities server-capabilities))
    ))

(define (shutdown server-instance id)
;;todo: kill server
  (if (null? (server-mutex server-instance))
    (server-shutdown?-set! server-instance #t)
    (with-mutex (server-mutex server-instance)
      (server-shutdown?-set! server-instance #t)))
    (thread-pool-stop! (server-thread-pool server-instance))
    (success-response id '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init-server
    (case-lambda
        [() (init-server (standard-input-port) (standard-output-port) (current-output-port))]
        [(input-port output-port log-port) 
          (let ([server-instance 
                  (if threaded?
                    (make-server input-port output-port log-port (init-thread-pool 4 #t) (make-mutex) '() #f)
                    (make-server input-port output-port log-port '() '() '() #f)) ])
            (let loop ([message (read-message server-instance)])
              (if (null? (server-thread-pool server-instance))
                (process-request server-instance message)
                (thread-pool-add-job (server-thread-pool server-instance) (lambda() (process-request server-instance message))))
              (loop (read-message server-instance)))
            (newline)
            (newline)
            (display "bye")
            (newline))]))
)
