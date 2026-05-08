#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
(import 
  (chezscheme)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders syntax-rules)
  (scheme-langserver analysis tokenizer))

(define (make-sr-node expr)
  (let* ((str (with-output-to-string (lambda () (pretty-print expr))))
         (annotations (source-file->annotations str "/dev/null"))
         (def-node (init-index-node #f (car annotations)))
         (def-children (index-node-children def-node)))
    (car (reverse def-children))))

(define (ne node)
  (annotation-stripped (index-node-datum/annotations node)))

(define expr2 (list 'define-syntax 'test2
                (list 'syntax-rules '()
                  (list '(_ 'x)
                    (list 'let '()
                      (list 'define-syntax 'double (list 'syntax-rules '() (list '(_ 'y) (list '+ 'y 'y))))
                      (list 'double 'x))))))

(let* ((n2 (make-sr-node expr2))
       (clause-index-nodes (cddr (index-node-children n2)))
       (clause-node (car clause-index-nodes))
       (clause-expression (ne clause-node))
       (template-expression (car (reverse clause-expression))))
  (display "template: ") (write template-expression) (newline)
  (display "has-nested?: ") (display (private:template-has-nested-macro? template-expression)) (newline)
  (display "find result: ")
  (display (find (lambda (c) (private:template-has-nested-macro? (car (reverse (ne c))))) clause-index-nodes))
  (newline))

(exit 0)
