(library (scheme-langserver analysis tokenizer)
  (export 
    source-file->annotations)
  (import 
    (chezscheme) 
    (scheme-langserver util io)
    (scheme-langserver util try))

;;many codes are from chez scheme read.ss
(define source-file->annotations
  (case-lambda
    ([path] (source-file->annotations (read-string path) path))
    ([source path]
    (let ([port (open-string-input-port source)]
        [source-file-descriptor (make-source-file-descriptor path (open-file-input-port path))])
      (let loop ([position (port-position port)][result '()])
        (try
          (let-values ([(ann end-pos) (get-datum/annotations port source-file-descriptor position)]) 
            (if (= position (port-position port))
              (filter annotation? result)
              (loop (port-position port) (append result `(,ann)))))
          (except e
            [else 
              (pretty-print `(format ,(condition-message e) ,@(condition-irritants e)))
              (pretty-print path)
              '()])))))))
)