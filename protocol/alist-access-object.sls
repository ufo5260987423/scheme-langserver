(library (scheme-langserver protocol message)
  (export 
    position-line
    position-character
    position?
    alist->position
    position->alist
    )
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

(define-record-type diagnostic
  (fields 
    (immutable range)
    (immutable severity)
    (immutable code)
    (immutable source)
    (immutable message)
    (immutable related-info)))

(define (alist->position alist)
  (make-postion (assq 'line alist) (assq 'character alist)))

(define (position->alist instance)
  (make-alist 'line (postion-line instance) 'character (postion-character instance)))

(define (alist->range alist)
  (make-range (assq 'start alist) (assq 'end alist)))

(define (range->alist instance)
  (make-alist 'start (range-start instance) 'end (range-end instance)))

(define (alist->text-edit alist)
  (make-text-edit (alist->range (assq 'range alist)) (assq 'text alist)))

(define (text-edit->alist instance)
  (make-alist 'range (range->alist (text-edit-range instance)) 'text (text-edit-text instance)))

(define (text-edit->alist-with-newText instance)
  (make-alist 'range (range->alist (text-edit-range instance)) 'newText (text-edit-text instance)))

(define (alist->location alist)
  (make-location (assq 'uri alist) (alist->range (assq 'range alist))))

(define (location->alist instance)
  (make-alist 'uri (location-uri instance) 'range (range->alist (location-range instance))))

(define (alist->location alist)
  (make-location (assq 'uri alist) (alist->range (assq 'range alist))))

(define (location->alist instance)
  (make-alist 'uri (location-uri instance) 'range (range->alist (location-range instance))))

(define (alist->text-document alist)
  (make-text-document (assq 'uri alist) (assq 'languageId alist) (assq 'version alist) (assq 'text alist)))

(define (text-document->alist instance)
  (make-alist 'uri (text-document-uri instance) 'languageId (text-document-language-id instance) 'version (text-document-version instance) 'text (text-document-text instance)))


(define (source-properties->position where)
  (make-position (assoc-ref where 'line) (assoc-ref where 'column)))


(define (alist->diagnostic alist)
  (make-diagnostic 
    (alist->range (assq 'range alist)) (assq 'severity alist) (assq 'code alist) (assq 'source alist)
    (assq 'code alist) (assq 'source alist) (assq 'message alist) (assq 'relatedInfo alist)))

(define (diagnostic->alist instance)
  (make-alist 'range (range->alist (diagnostic-range instance)) 'severity (diagnostic-severtiy instance) 
      'code (diagnostic-code instance) 'source (diagnostic-source instance)
      'message (diagnostic-message instance) 'relatedInfo (diagnostic-related-info instance)))
)