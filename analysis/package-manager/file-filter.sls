(library (scheme-langserver analysis package-manager file-filter)
  (export 
    make-extension-filter
    make-scheme-file-filter
    file-filter->predicate
    file-filter-config?
    file-filter-config-serializable?)
  (import 
    (chezscheme)
    (only (srfi :13 strings) string-suffix?)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis package-manager txt-filter))

;; Returns a predicate that accepts directories and files ending with one of
;; the given extensions.
(define (make-extension-filter extensions)
  (lambda (path)
    (or (file-directory? path)
        (ormap (lambda (ext) (string-suffix? ext path)) extensions))))

;; Standard Scheme file extensions.  Includes .sld because R7RS libraries
;; commonly use it.
(define (make-scheme-file-filter)
  (make-extension-filter '(".sps" ".sls" ".scm" ".ss" ".sld")))

;; Convert a file-filter configuration into a predicate suitable for use as a
;; workspace facet.
;;
;; Config values:
;;   'akku      -- Akku package-manager filter driven by .akku/list
;;   'txt       -- .scm.txt files only (used by test fixtures)
;;   'scheme    -- all standard Scheme extensions
;;   procedure  -- used directly as a custom facet
;;   list of strings -- exact extension list, e.g. '(".sls" ".scm")
(define (file-filter->predicate config path)
  (cond
    [(procedure? config)
     (lambda (path)
       (or (file-directory? path)
           (config path)))]
    [(eq? config 'akku)
     (generate-akku-acceptable-file-filter (string-append path "/.akku/list"))]
    [(eq? config 'txt) (generate-txt-file-filter)]
    [(eq? config 'scheme) (make-scheme-file-filter)]
    [(and (list? config) (andmap string? config))
     (if (null? config)
       (generate-akku-acceptable-file-filter (string-append path "/.akku/list"))
       (make-extension-filter config))]
    [else
      ;; Be permissive for unknown symbols: fall back to Akku so existing
      ;; callers that passed an arbitrary symbol keep working.
      (generate-akku-acceptable-file-filter (string-append path "/.akku/list"))]))

;; Is the given value a valid file-filter configuration?
(define (file-filter-config? x)
  (or (symbol? x)
      (procedure? x)
      (and (list? x) (andmap string? x))))

;; Can the configuration be stored verbatim in a cache manifest?  Procedure
;; filters cannot be serialized, so they disable cache loading/saving.
(define (file-filter-config-serializable? x)
  (or (symbol? x)
      (and (list? x) (andmap string? x))))
)
