(library (scheme-langserver util path)
    (export 
      path->uri 
      uri->path 
      uri-is-path? 
      uri->name
      path->name)
    (import 
      (chezscheme)
      (only (chibi pathname) path-strip-directory)
      (scheme-langserver util environment) 
      (only (srfi :13 strings) string-index string-prefix? string-suffix? string-drop string-drop-right string-contains string-join)
      (srfi :14))

;; reserved = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "," | "$"
;; escaped     = "%" hex hex
;; hex         = digit | "A" | "B" | "C" | "D" | "E" | "F" |
;;                       "a" | "b" | "c" | "d" | "e" | "f"
;;%= %25
;; reserved = "/" | "?" | ";" | "="

;; Split a string by a single character delimiter.
(define (private:string-split str char)
  (let loop ([start 0] [result '()])
    (let ([pos (string-index str char start)])
      (if pos
        (loop (+ 1 pos) (cons (substring str start pos) result))
        (reverse (cons (substring str start (string-length str)) result))))))

;; Resolve a relative path against a base directory, normalising "." and "..".
;; Both base and the result are absolute paths.
(define (private:resolve-relative-path base path)
  (let* ([base-parts (private:string-split base #\/)]
         ;; Remove empty strings caused by leading or trailing slashes
         [clean-base (filter (lambda (s) (not (string=? s ""))) base-parts)]
         [path-parts (private:string-split path #\/)]
         [stack clean-base])
    (for-each
      (lambda (seg)
        (cond
          [(or (string=? seg "") (string=? seg "."))
           (void)]
          [(string=? seg "..")
           (if (null? stack)
             (void)
             (set! stack (reverse (cdr (reverse stack)))))]
          [else
            (set! stack (append stack (list seg)))]))
      path-parts)
    (string-append "/" (string-join stack "/"))))

(define (private:path->uri-transformation path)
  (let ([token-vector (list->vector (string->list path))]
      [start-position (string-index path (list->char-set 
        '(#\% #\? #\;
          #\=)))])
    (if start-position
      (string-append 
        (substring path 0 start-position)
        "%"
        (private:char->hex-string (vector-ref token-vector start-position))
        (if (< start-position (string-length path))
          (private:path->uri-transformation (substring path (+ 1 start-position) (string-length path)))
          ""))
      path)))

(define (path->uri path)
  (string-append
    "file://"
    (private:path->uri-transformation
      (if (path-absolute? path)
        path
        (private:resolve-relative-path (current-directory) path)))))

(define (uri->path uri)
  (let* ([rest (substring uri (+ 3 (string-contains uri "://")) (string-length uri))]
      [?-position (string-contains rest "?")]
      [target (substring rest 0 (if ?-position ?-position (string-length rest)))]
      [end (string-length target)])
    (let loop ([position 0]
        [next-position (string-index target #\%)])
      (cond 
        [(>= position end) ""]
        [next-position 
          (string-append 
            (substring target position next-position)
            (string 
              (integer->char 
                (string->number 
                  (string-append "#x" 
                  (substring target (+ next-position 1) (+ next-position 3))))))
            (loop (+ next-position 3) (string-index target #\% (+ next-position 3))))]
        [else (substring target position end)]))))

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->name uri)
  (path->name (uri->path uri)))

(define (path->name path)
  (if (string-suffix? "/" path )
    (path-strip-directory (string-drop-right path 1))
    (path-strip-directory path)))

;; "%"| "/" | "?" | ";" | "="
(define (private:char->hex-string char)
  (case char
    (#\% "25")
    (#\/ "2F")
    (#\; 
      "3B")
    (#\= "3D")
    (#\? "3F")))
)