(library (scheme-langserver)
  (export 
    init-server)
  (import 
    (chezscheme) 
    (ufo-thread-pool) 
    (ufo-try) 
    (ufo-timer) 

    (scheme-langserver analysis workspace)

    (scheme-langserver protocol error-code) 
    (scheme-langserver protocol request)
    (scheme-langserver protocol response)
    (scheme-langserver protocol server)
    (scheme-langserver protocol analysis request-queue)

    (scheme-langserver protocol apis references)
    (scheme-langserver protocol apis formatting)
    (scheme-langserver protocol apis document-highlight)
    (scheme-langserver protocol apis completion)
    (scheme-langserver protocol apis hover)
    (scheme-langserver protocol apis definition)
    (scheme-langserver protocol apis document-sync)
    (scheme-langserver protocol apis document-symbol)
    (scheme-langserver protocol apis document-diagnostic)
    (scheme-langserver protocol apis file-change-notification)

    (scheme-langserver util association)
    (scheme-langserver util path))

(define (private:try-catch server-instance request)
  (let ([method (request-method request)]
      [id (request-id request)])
    (try 
      (process-request server-instance request)
      (except c 
        [(condition? c) 
          (do-log 
            (string-append "error: " (with-output-to-string (lambda () (pretty-print `(format ,(condition-message c) ,@(condition-irritants c))))))
            server-instance)
          (do-log-timestamp server-instance)
          (send-message server-instance (fail-response id unknown-error-code method))]
        [else 
          (do-log 
            (with-output-to-string (lambda () (pretty-print c)))
            server-instance)
          (do-log-timestamp server-instance) 
          (send-message server-instance (fail-response id unknown-error-code method))]))))

(define (private:publish-diagnostics server-instance)
  (if (not (null? (server-workspace server-instance)))
    (map 
      (lambda (params)
        (send-message server-instance (make-notification "textDocument/publishDiagnostics"  params) 'publish))
      (unpublish-diagnostics->list (server-workspace server-instance)))))

(define (process-request server-instance request)
  (let* ([method (request-method request)]
        [id (request-id request)]
        [params (request-params request)]
        [workspace (server-workspace server-instance)])
    (if 
      (and 
        (server-shutdown? server-instance)
        (not (equal? "initialize" method)))
      (send-message server-instance (fail-response id server-not-initialized "not initialized"))
      (case method
        ["initialize" (send-message server-instance (initialize server-instance id params))] 
        ["initialized" '()] 
        ["private:publish-diagnoses" (private:publish-diagnostics server-instance)] 

        ["textDocument/didOpen" (did-open workspace params)]
        ["textDocument/didClose" (did-close workspace params)]
        ["textDocument/didChange" (did-change workspace params)]

        ["workspace/didCreateFiles" (did-create workspace params)]
        ["workspace/didRenameFiles" (did-rename workspace params)]
        ["workspace/didDeleteFiles" (did-delete workspace params)]

        ["textDocument/hover" (send-message server-instance (success-response id (hover workspace params)))]
        ["textDocument/completion" (send-message server-instance (success-response id (completion workspace params)))]
        ["textDocument/references" (send-message server-instance (success-response id (find-references workspace params)))]
        ; ["textDocument/documentHighlight" 
        ;   (try
        ;     (send-message server-instance (success-response id (find-highlight workspace params)))
        ;     (except c
        ;       [else 
        ;         (do-log `(format ,(condition-message c) ,@(condition-irritants c)) server-instance)
                ; (do-log-timestamp server-instance)
        ;         (send-message server-instance (fail-response id unknown-error-code method))]))]
          ; ["textDocument/signatureHelp"
          ;  (text-document/signatureHelp id params)]
        ["textDocument/definition" (send-message server-instance (success-response id (definition workspace params)))]
        ["textDocument/documentSymbol" (send-message server-instance (success-response id (document-symbol workspace params)))]
        ["textDocument/diagnostic" (send-message server-instance (success-response id (diagnostic workspace params)))]
        ;TODO: pretty-format with comments
        ; ["textDocument/formatting"
        ;   (try
        ;     (send-message server-instance (success-response id (formatting workspace params)))
        ;     (except c
        ;       [else 
        ;         (do-log `(format ,(condition-message c) ,@(condition-irritants c)) server-instance)
        ;         (do-log-timestamp server-instance)
        ;         (send-message server-instance (fail-response id unknown-error-code method))]))]

        ["$/cancelRequest" (send-message server-instance (fail-response id request-cancelled (assoc-ref params 'method)))]
          ; ["textDocument/prepareRename"
          ;  (text-document/prepareRename id params)]
          ; ["textDocument/rangeFormatting"
          ;  (text-document/range-formatting! id params)]
          ; ["textDocument/onTypeFormatting"
          ;  (text-document/on-type-formatting! id params)]
          ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didChangeWatchedFilesClientCapabilities
        [else (send-message server-instance (fail-response id method-not-found (string-append "invalid request for method " method "\n")))]))))
	; public static final string text_document_code_lens = "textdocument/codelens";
	; public static final string text_document_signature_help = "textdocument/signaturehelp";
	; public static final string text_document_rename = "textdocument/rename";
	; public static final string workspace_execute_command = "workspace/executecommand";
	; public static final string workspace_symbol = "workspace/symbol";
	; public static final string workspace_watched_files = "workspace/didchangewatchedfiles";
	; public static final string code_action = "textdocument/codeaction";
	; public static final string typedefinition = "textdocument/typedefinition";
	; public static final string document_highlight = "textdocument/documenthighlight";
	; public static final string foldingrange = "textdocument/foldingrange";
	; public static final string workspace_change_folders = "workspace/didchangeworkspacefolders";
	; public static final string implementation = "textdocument/implementation";
	; public static final string selection_range = "textdocument/selectionrange";

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initialize server-instance id params)
  (let* ([root-path (uri->path (assq-ref params 'rootUri))]
        [client-capabilities (assq-ref params 'capabilities)]
        [window (assq-ref client-capabilities 'window)]
        [workDoneProgress? (if window (assq-ref window 'workDoneProgress) #f)]
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
              ; 'diagnosticProvider (make-alist 'interFileDependencies #t 'workspaceDiagnostics #f)
              ; 'workspaceSymbol #t
              ; 'typeDefinitionProvider #t
              ; 'selectionRangeProvider #t
              ; 'callHierarchyProvider #t
              'completionProvider (make-alist 'triggerCharacters (vector))
              ; 'signatureHelpProvider (make-alist 'triggerCharacters (vector " " ")" "]"))
              ; 'implementationProvider #t
              ; 'renameProvider renameProvider
              ; 'codeActionProvider #t
              ; 'documentHighlightProvider #t
              'documentSymbolProvider #t
              ; 'documentLinkProvider #t
              'documentFormattingProvider #f
              'workspace 
                (make-alist 'fileOperations 
                  (make-alist 
                  ;however, these three are only triggered when create/rename/delete file with vscode's origin create/renmae/delete
                  ;so that we must to add fault tolerant to re-init workspace
                    'didCreate (make-alist 'filters (vector (make-alist 'scheme "file" 'pattern (make-alist 'glob "**/*"))))
                    'didRename (make-alist 'filters (vector (make-alist 'scheme "file" 'pattern (make-alist 'glob "**/*"))))
                    'didDelete (make-alist 'filters (vector (make-alist 'scheme "file" 'pattern (make-alist 'glob "**/*"))))))
              ; 'documentRangeFormattingProvider #f
              ; 'documentOnTypeFormattingProvider (make-alist 'firstTriggerCharacter ")" 'moreTriggerCharacter (vector "\n" "]"))
              ; 'codeLensProvider #t
              ; 'foldingRangeProvider #t
              ; 'colorProvider #t
              ; 'workspace workspace-configuration
              )])

    (if (null? (server-mutex server-instance))
      (begin 
        (server-workspace-set! server-instance (init-workspace root-path 'akku (server-top-environment server-instance) #f (server-type-inference? server-instance)))
        (server-work-done-progress?-set! server-instance workDoneProgress?)
        (success-response id (make-alist 'capabilities server-capabilities)))
      (with-mutex (server-mutex server-instance) 
        (if (null? (server-workspace server-instance))
          (begin 
            (server-workspace-set! server-instance (init-workspace root-path 'akku (server-top-environment server-instance) #t (server-type-inference? server-instance)))
            (server-work-done-progress?-set! server-instance workDoneProgress?)
            (success-response id (make-alist 'capabilities server-capabilities)))
          (fail-response id server-error-start "server has been initialized"))))))

(define init-server
    (case-lambda
        [() 
          (init-server 
            (standard-input-port) 
            (standard-output-port) 
            '() 
            #f
            #f
            'r6rs)]
        [(log-path) 
          (init-server 
            (standard-input-port) 
            (standard-output-port) 
            (open-file-output-port 
              log-path 
              (file-options replace) 
              'block 
              (make-transcoder (utf-8-codec))) 
            #f
            #f
            'r6rs)]
        [(log-path enable-multi-thread?) 
          (init-server log-path enable-multi-thread? #f)]
        [(log-path enable-multi-thread? type-inference?)
          (init-server 
            (standard-input-port) 
            (standard-output-port) 
            (open-file-output-port 
              log-path 
              (file-options replace) 
              'block 
              (make-transcoder (utf-8-codec))) 
            (equal? enable-multi-thread? "enable")
            (equal? type-inference? "enable")
            'r6rs)]
        [(input-port output-port log-port enable-multi-thread?) 
          (init-server input-port output-port log-port enable-multi-thread? #f 'r6rs)]
        [(input-port output-port log-port enable-multi-thread? type-inference?) 
          (init-server input-port output-port log-port enable-multi-thread? type-inference? 'r6rs)]
        [(input-port output-port log-port enable-multi-thread? type-inference? top-environment)
          ;The thread-pool size just limits how many threads to process requests;
          (let* ([thread-pool (if (and enable-multi-thread? threaded?) (init-thread-pool 2 #t) '())]
              [request-queue (if (and enable-multi-thread? threaded?) (make-request-queue) '())]
              [server-instance (make-server input-port output-port log-port thread-pool request-queue '() type-inference? top-environment)]
              [request-processor (lambda (r) (private:try-catch server-instance r))]
              [interval-timer 
                (if (and enable-multi-thread? threaded?) 
                  (init-interval-timer 
                    (make-time 'time-duration 0 1)
                    (lambda () 
                      (request-queue-push request-queue (make-request '() "private:publish-diagnoses" '()) request-processor (server-workspace server-instance)))
                    (lambda () (not (server-shutdown? server-instance)))
                    thread-pool)
                  '())])
            (if (not (null? thread-pool)) 
              (begin 
                (start-timer interval-timer)
                (thread-pool-add-job thread-pool 
                  (lambda () 
                    (let loop ()
                      ((request-queue-pop request-queue request-processor))
                      (if (not (and (server-shutdown? server-instance) (request-queue-empty? request-queue))) (loop)))))))
            (let loop ([request-message (read-message server-instance)])
              (cond 
                [(null? request-message) '()]
                [(or (equal? "shutdown" (request-method request-message)) (equal? "exit" (request-method request-message))) 
                  (server-shutdown?-set! server-instance #t)
                  '()]
                [(null? thread-pool) 
                  (request-processor request-message)
                  (loop (read-message server-instance))]
                [else
                  (request-queue-push request-queue request-message request-processor (server-workspace server-instance))
                  (loop (read-message server-instance))]))
            server-instance)]))
)
