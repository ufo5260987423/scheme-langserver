(library (scheme-langserver analysis bad-brackets-scanner)
  (export
    compute-bad-brackets)
  (import
    (chezscheme))

  (define (compute-bad-brackets source)
    (let ([port (open-input-string source)])
      (scan port '() '())))

  ;;; Main scan loop
  ;;; port: input port over the source string
  ;;; bad:  list of bad bracket positions (0-based) found so far
  ;;; stack: opener stack, each element is (position . char)
  (define (scan port bad stack)
    (let ([c (read-char port)])
      (cond
        [(eof-object? c)
          ;;; EOF: remaining openers on stack are unmatched
          (append (map car stack) bad)]
        [(char=? c #\")
          (skip-string port)
          (scan port bad stack)]
        [(char=? c #\;)
          (skip-line-comment port)
          (scan port bad stack)]
        [(char=? c #\#)
          (handle-sharp port bad stack)]
        [(char=? c #\()
          (let ([pos (- (port-position port) 1)])
            (scan port bad (cons (cons pos #\() stack)))]
        [(char=? c #\[)
          (let ([pos (- (port-position port) 1)])
            (scan port bad (cons (cons pos #\[) stack)))]
        [(char=? c #\))
          (let ([pos (- (port-position port) 1)])
            (let-values ([(new-bad new-stack)
                           (handle-close pos stack #\( bad)])
              (scan port new-bad new-stack)))]
        [(char=? c #\])
          (let ([pos (- (port-position port) 1)])
            (let-values ([(new-bad new-stack)
                           (handle-close pos stack #\[ bad)])
              (scan port new-bad new-stack)))]
        [else
          (scan port bad stack)])))

  ;;; Handle a closer: search upward in the stack for the nearest
  ;;; opener of the matching type.  Any intervening openers of the
  ;;; opposite type are marked as bad (orphaned).
  ;;;
  ;;; close-pos: 0-based position of the closing bracket
  ;;; stack:     current opener stack
  ;;; open-char: expected opener character (#\( or #\[)
  ;;; bad:       accumulated bad positions
  ;;; Returns:   (new-bad . new-stack)
  (define (handle-close close-pos stack open-char bad)
    (let search ([s stack] [prefix '()])
      (cond
        [(null? s)
          ;;; No matching opener found: this closer is bad.
          (values (cons close-pos bad) stack)]
        [(char=? open-char (cdar s))
          ;;; Found matching opener.  Everything between it and the
          ;;; top of the stack (prefix) is of the opposite type and
          ;;; gets orphaned.
          (let ([orphaned (map car (reverse prefix))])
            (values (append orphaned bad) (cdr s)))]
        [else
          (search (cdr s) (cons (car s) prefix))])))

  ;;; Skip a double-quoted string, handling backslash escapes.
  (define (skip-string port)
    (let loop ([c (read-char port)])
      (cond
        [(eof-object? c) 'done]
        [(char=? c #\\)
          (read-char port)                ; consume escaped char
          (loop (read-char port))]
        [(char=? c #\") 'done]
        [else (loop (read-char port))])))

  ;;; Skip a line comment from ; to end of line.
  (define (skip-line-comment port)
    (let loop ([c (read-char port)])
      (cond
        [(eof-object? c) 'done]
        [(char=? c #\newline) 'done]
        [(char=? c #\return)
          (when (eqv? (lookahead-char port) #\newline)
            (read-char port))
          'done]
        [else (loop (read-char port))])))

  ;;; Skip one datum (used for #; datum comments).
  ;;; Recursively skips nested bracketed expressions, strings,
  ;;; block comments, and line comments.
  (define (skip-datum port)
    (let skip-ws ()
      (let ([c (peek-char port)])
        (cond
          [(eof-object? c) 'done]
          [(char-whitespace? c) (read-char port) (skip-ws)]
          [(char=? c #\;) (skip-line-comment port) (skip-ws)]
          [(char=? c #\#)
           (read-char port)
           (let ([next (peek-char port)])
             (cond
               [(eof-object? next) 'done]
               [(char=? next #\|) (read-char port) (skip-block-comment port) (skip-ws)]
               [(char=? next #\\)
                (read-char port) (read-char port)
                (let ([ch (peek-char port)])
                  (when (and (not (eof-object? ch)) (char-alphabetic? ch))
                    (let loop ([n (peek-char port)])
                      (when (and (not (eof-object? n)) (char-alphabetic? n))
                        (read-char port) (loop (peek-char port))))))
                'done]
               [(or (char=? next #\() (char=? next #\[))
                (read-char port) (skip-bracketed port)]
               [else 'done]))]
          [(or (char=? c #\() (char=? c #\[))
           (read-char port) (skip-bracketed port)]
          [(char=? c #\")
           (read-char port) (skip-string port)]
          [else
           (let loop ()
             (let ([c (peek-char port)])
               (cond
                 [(eof-object? c) 'done]
                 [(char-whitespace? c) 'done]
                 [(memv c '(#\( #\) #\[ #\] #\" #\; #\#)) 'done]
                 [else (read-char port) (loop)])))]))))

  (define (skip-bracketed port)
    (let loop ([depth 1])
      (when (> depth 0)
        (let ([c (read-char port)])
          (cond
            [(eof-object? c) 'done]
            [(or (char=? c #\() (char=? c #\[)) (loop (+ depth 1))]
            [(or (char=? c #\)) (char=? c #\])) (loop (- depth 1))]
            [(char=? c #\") (skip-string port) (loop depth)]
            [(char=? c #\;) (skip-line-comment port) (loop depth)]
            [(char=? c #\#)
             (let ([next (peek-char port)])
               (cond
                 [(eof-object? next) 'done]
                 [(char=? next #\|) (read-char port) (skip-block-comment port) (loop depth)]
                 [else (loop depth)]))]
            [else (loop depth)])))))

  ;;; Handle # prefix: character literals, block comments, datum comments,
  ;;; or other sharp-sign syntax whose brackets we still need to track.
  (define (handle-sharp port bad stack)
    (let ([c (peek-char port)])
      (cond
        [(eof-object? c)
          (scan port bad stack)]
        [(char=? c #\\)
          ;;; Character literal #\...
          (read-char port)                  ; consume backslash
          (let ([ch (read-char port)])
            (when (char-alphabetic? ch)
              ;;; Multi-character names like #\space, #\newline
              (let loop ([n (peek-char port)])
                (when (and (not (eof-object? n)) (char-alphabetic? n))
                  (read-char port)
                  (loop (peek-char port))))))
          (scan port bad stack)]
        [(char=? c #\|)
          ;;; Block comment #| ... |#
          (read-char port)                  ; consume |
          (skip-block-comment port)
          (scan port bad stack)]
        [(char=? c #\;)
          ;;; Datum comment #; — skip the next datum
          (read-char port)                  ; consume ;
          (skip-datum port)
          (scan port bad stack)]
        [else
          ;;; Everything else (#(, #[, #vu8, #t, #f, etc.) — the
          ;;; following characters will be handled by the main loop.
          (scan port bad stack)])))

  ;;; Skip a nested block comment #| ... |#.
  (define (skip-block-comment port)
    (let loop ([c (read-char port)])
      (cond
        [(eof-object? c) 'done]
        [(and (char=? c #\|) (eqv? (lookahead-char port) #\#))
          (read-char port)                  ; consume #
          'done]
        [(and (char=? c #\#) (eqv? (lookahead-char port) #\|))
          (read-char port)                  ; consume |
          (skip-block-comment port)         ; nested comment
          (loop (read-char port))]
        [else (loop (read-char port))])))
)
