(library (scheme-langserver util text)
  (export 
    get-line-end-position
    text+position->int)
  (import 
    (rnrs)
    (only (srfi :13 strings) string-index))

(define (get-line-end-position text start-position)
  (let ([NL (string-index text #\newline start-position)]
      [RE (string-index text #\return start-position)])
    (cond
      [(and NL RE) (if (= 1 (abs (- NL RE))) (max NL RE) (min NL RE))]
      [NL NL]
      [RE RE]
      [else (string-length text)])))

(define (text+position->int text line offset)
  (let loop ([current-bias 0]
      [current-line 0])
    (let* ([current-line-end-position (get-line-end-position text current-bias)]
        [maybe-result (+ current-bias offset)])
      (cond
        [(< (string-length text) current-bias) (raise 'postion-out-of-range)]
        [(< current-line line) (loop (+ 1 current-line-end-position) (+ 1 current-line))]
        [(and (= current-line line) (<= maybe-result current-line-end-position)) maybe-result]
        [else (raise 'position-out-of-range)]))))
)