(library (scheme-langserver protocol alist-access-object)
  (export 
    position-line
    position-character
    position?
    alist->position
    position->alist
    make-position

    alist->diagnostic
    diagnostic->alist

    alist->text-document
    text-document-text
    text-document-uri
    text-document-language-id
    text-document-version

    make-text-edit
    alist->text-edit 
    text-edit->alist-with-newText
    text-edit-range
    text-edit-text

    make-range
    range-start
    range-end

    int+text->position
    text+position->int

    location->alist
    make-location

    versioned-text-document-identifier-uri
    versioned-text-document-identifier-version
    alist->versioned-text-document-identifier 
    versioned-text-document-identifier->alist

    document-symbol->alist
    make-document-symbol)
  (import 
    (chezscheme) 
    (scheme-langserver util association)
    (only (srfi :13 strings) string-index))

(define-record-type position
  (fields 
    (immutable line)
    (immutable character)))

(define-record-type range
  (fields 
    (immutable start)
    (immutable end)))

(define-record-type text-edit
  (fields 
    (immutable range)
    (immutable text)))

(define-record-type location
  (fields 
    (immutable uri)
    (immutable range)))

(define-record-type document-symbol
  (fields 
    (immutable name)
    (immutable kind)
    (immutable range)
    (immutable selectionRange)))

(define-record-type text-document
  (fields 
    (immutable uri)
    (immutable language-id)
    (immutable version)
    (immutable text)))

(define-record-type versioned-text-document-identifier
  (fields 
    (immutable uri)
    (immutable version)))

(define-record-type diagnostic
  (fields 
    (immutable range)
    (immutable severity)
    (immutable code)
    (immutable source)
    (immutable message)
    (immutable related-info)))

(define (get-line-end-position text start-position)
  (let ([NL (string-index text #\newline start-position)]
      [RE (string-index text #\return start-position)])
    (cond
      [(and NL RE) (if (= 1 (abs (- NL RE))) (max NL RE) (min NL RE))]
      [NL NL]
      [RE RE]
      [else (string-length text)])))

(define (int+text->position bias text)
  (let loop ([current-bias 0]
        [current-line 0])
    (let ([current-line-end-index (get-line-end-position text current-bias)])
      (cond
        [(< (string-length text) current-bias) (raise 'postion-out-of-range)]
        [(< current-line-end-index bias)
          (loop (+ current-line-end-index 1) (+ 1 current-line))]
        [(>= current-line-end-index bias) 
          (make-position current-line (- bias current-bias))]
        [else (raise 'position-out-of-range)]))))

(define (text+position->int text position)
  (let loop ([current-bias 0]
      [current-line 0])
    (let* ([current-line-end-position (get-line-end-position text current-bias)]
        [maybe-result (+ current-bias (position-character position))])
      (cond
        [(< (string-length text) current-bias) (raise 'postion-out-of-range)]
        [(< current-line (position-line position)) (loop (+ 1 current-line-end-position) (+ 1 current-line))]
        [(and (= current-line (position-line position)) (<= maybe-result current-line-end-position)) maybe-result]
        [else (raise 'position-out-of-range)]))))

(define (alist->document-symbol alist)
  (make-document-symbol 
    (assq-ref alist 'name) 
    (assq-ref alist 'kind) 
    (alist->range (assq-ref 'range))
    (alist->range (assq-ref 'selectionRange))))

(define (document-symbol->alist document-symbol)
  (make-alist 
    'name (document-symbol-name document-symbol)
    'kind (document-symbol-kind document-symbol)
    'range (range->alist (document-symbol-range document-symbol))
    'selectionRange (range->alist (document-symbol-selectionRange document-symbol))))

(define (alist->versioned-text-document-identifier alist)
  (make-versioned-text-document-identifier (assq-ref alist 'uri) (assq-ref alist 'version)))

(define (versioned-text-document-identifier->alist instance)
  (make-alist 'version (versioned-text-document-identifier-version instance) 'uri (versioned-text-document-identifier-uri instance)))

(define (alist->position alist)
  (make-position (assq-ref alist 'line) (assq-ref alist 'character)))

(define (position->alist instance)
  (make-alist 'line (position-line instance) 'character (position-character instance)))

(define (alist->range alist)
  (make-range (alist->position (assq-ref alist 'start)) (alist->position (assq-ref alist 'end))))

(define (range->alist instance)
  (make-alist 'start (position->alist (range-start instance)) 'end (position->alist (range-end instance))))

(define (alist->text-edit alist)
  (make-text-edit 
    (if (null? (assq-ref alist 'range))
      '()
      (alist->range (assq-ref alist 'range)))
    (assq-ref alist 'text)))

(define (text-edit->alist instance)
  (make-alist 'range (range->alist (text-edit-range instance)) 'text (text-edit-text instance)))

(define (text-edit->alist-with-newText instance)
  (make-alist 'range (range->alist (text-edit-range instance)) 'newText (text-edit-text instance)))

(define (alist->location alist)
  (make-location (assq-ref alist 'uri) (alist->range (assq-ref alist 'range))))

(define (location->alist instance)
  (make-alist 'uri (location-uri instance) 'range (range->alist (location-range instance))))

(define (alist->text-document alist)
  (make-text-document (assq-ref alist 'uri) (assq-ref alist 'languageId) (assq-ref alist 'version) (assq-ref alist 'text)))

(define (text-document->alist instance)
  (make-alist 'uri (text-document-uri instance) 'languageId (text-document-language-id instance) 'version (text-document-version instance) 'text (text-document-text instance)))


(define (source-properties->position where)
  (make-position (assoc-ref where 'line) (assoc-ref where 'column)))


(define (alist->diagnostic alist)
  (make-diagnostic 
    (alist->range (assq-ref alist 'range)) 
    (assq-ref alist 'severity) 
    (assq-ref alist 'code) 
    (assq-ref alist 'source)
    (assq-ref alist 'message) 
    (assq-ref alist 'relatedInfo)))

(define (diagnostic->alist instance)
  (make-alist 
    'range (range->alist (diagnostic-range instance)) 
    'severity (diagnostic-severity instance) 
    'code (diagnostic-code instance) 
    'source (diagnostic-source instance)
    'message (diagnostic-message instance) 
    'relatedInfo (diagnostic-related-info instance)))
)
