(library (scheme-langserver protocol alist-access-object)
  (export 
    position-line
    position-character
    position?
    alist->position
    position->alist

    alist->diagnostic
    diagnostic->alist

    alist->text-document
    text-document-text
    text-document-uri
    text-document-language-id
    text-document-version

    alist->text-edit 
    text-edit-range
    text-edit-text

    range-start
    range-end

    versioned-text-document-identifier-uri
    versioned-text-document-identifier-version
    alist->versioned-text-document-identifier 
    versioned-text-document-identifier->alist)
  (import (rnrs) (scheme-langserver util association))

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

(define (alist->versioned-text-document-identifier alist)
  (make-versioned-text-document-identifier (assq-ref alist 'uri) (assq-ref alist 'version)))

(define (versioned-text-document-identifier->alist instance)
  (make-alist 'version (versioned-text-document-identifier-version instance) 'uri (versioned-text-document-identifier-uri instance)))

(define (alist->position alist)
  (make-position (assq-ref alist 'line) (assq-ref alist 'character)))

(define (position->alist instance)
  (make-alist 'line (position-line instance) 'character (position-character instance)))

(define (alist->range alist)
  (make-range (assq-ref alist 'start) (assq-ref alist 'end)))

(define (range->alist instance)
  (make-alist 'start (range-start instance) 'end (range-end instance)))

(define (alist->text-edit alist)
  (make-text-edit (alist->range (assq-ref alist 'range)) (assq-ref alist 'text)))

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