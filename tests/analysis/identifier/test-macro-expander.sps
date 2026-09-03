#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2024 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util path)
  (scheme-langserver util test)

  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)

  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier macro-expander)
  (scheme-langserver analysis tokenizer)
  (scheme-langserver analysis workspace))

(test-begin "expand:step-by-step & generate-pair:template+callee & generate-pair:template+expanded for match-index-node (syntax-rules)")
  (let* ([workspace-instance (init-workspace (current-directory))]
     [root-file-node (workspace-file-node workspace-instance)]
     [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/dependency/rules/library-import.sls"))]
     [document (file-node-document target-file-node)]
     [root-index-node (car (document-index-node-list document))]
     [match-clause-node (find-define-with-params root-index-node 'match-clause)]
     [target-index-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'match-index-node (car expr)))))
        match-clause-node)]
     [identifier-reference (car (find-available-references-for document target-index-node 'match-index-node))]
     [template+callees (generate-pair:template+callee identifier-reference target-index-node document)]
     [expanded-expression (car (expand:step-by-step identifier-reference target-index-node document))]
     [expanded-index-node
      (init-index-node
        (identifier-reference-initialization-index-node identifier-reference)
        (car
          (source-file->annotations
            (with-output-to-string (lambda () (pretty-print expanded-expression)))
            (uri->path (document-uri (identifier-reference-document identifier-reference))))))]
     [template+expanded (generate-pair:template+expanded identifier-reference expanded-index-node target-index-node document template+callees)])
     (test-equal
     '(match-steer index-node-match-protocol index-node
        (('only ((:= index-node-expression identifier) ...) . rest)
          identifier)
        (('except ((:= index-node-expression identifier) ...) . rest)
          identifier)
        (('prefix ((:= index-node-expression identifier) ...) . rest)
          identifier)
        (('rename ((:= index-node-expression identifier) ...) . rest)
          identifier)
        (('for ((:= index-node-expression identifier) ...) 'run . rest)
          identifier)
        (('for ((:= index-node-expression identifier) ...) ((:= index-node-expression (? (lambda (e) (equal? e '(meta 0))) :_))) . rest)
          identifier)
        (((:= index-node-expression identifier) ...) identifier)
        (else '()))
     expanded-expression)
     (test-equal
     '((_ . match-index-node)
        (expr . index-node)
        ((pattern ...) ('only ((:= index-node-expression identifier) ...) . rest)
          ('except ((:= index-node-expression identifier) ...) . rest)
          ('prefix ((:= index-node-expression identifier) ...) . rest)
          ('rename ((:= index-node-expression identifier) ...) . rest)
          ('for ((:= index-node-expression identifier) ...) 'run . rest)
          ('for ((:= index-node-expression identifier) ...) ((:= index-node-expression (? (lambda (e) (equal? e '(meta 0))) :_))) . rest)
          ((:= index-node-expression identifier) ...) else)
        ((body ...) identifier identifier identifier identifier identifier identifier identifier '()))
     (map
      (lambda (p)
        `(,(car p) .
          ,(if (index-node? (cdr p))
            (annotation-stripped (index-node-datum/annotations (cdr p)))
            (map (lambda (px) (annotation-stripped (index-node-datum/annotations px))) (cdr p)))))
      template+callees))

     (test-equal
     '((expr . index-node)
        (((pattern ...) ...) ('only ((:= index-node-expression identifier) ...) . rest)
          ('except ((:= index-node-expression identifier) ...) . rest)
          ('prefix ((:= index-node-expression identifier) ...) . rest)
          ('rename ((:= index-node-expression identifier) ...) . rest)
          ('for ((:= index-node-expression identifier) ...) 'run . rest)
          ('for ((:= index-node-expression identifier) ...) ((:= index-node-expression (? (lambda (e) (equal? e '(meta 0))) :_))) . rest)
          ((:= index-node-expression identifier) ...) else)
        (((body ...) ...) identifier identifier identifier identifier identifier identifier identifier '()))
     (map
      (lambda (p)
        `(,(car p) .
          ,(let ([d (cdr p)])
            (cond
              [(index-node? d)
                (annotation-stripped (index-node-datum/annotations d))]
              [(find index-node? d)
                (map
                  (lambda (a) (annotation-stripped (index-node-datum/annotations a)))
                  d)]
              [else
                (map
                  (lambda (a)
                    (map
                      (lambda (a) (annotation-stripped (index-node-datum/annotations a)))
                      a))
                  d)]))))
      template+expanded))

     (test-equal
     '((match-index-node) (index-node index-node)
        (('only ((:= index-node-expression identifier) ...) . rest)
          ('only ((:= index-node-expression identifier) ...) . rest))
        (('except ((:= index-node-expression identifier) ...) . rest)
          ('except ((:= index-node-expression identifier) ...) . rest))
        (('prefix ((:= index-node-expression identifier) ...) . rest)
          ('prefix ((:= index-node-expression identifier) ...) . rest))
        (('rename ((:= index-node-expression identifier) ...) . rest)
          ('rename ((:= index-node-expression identifier) ...) . rest))
        (('for ((:= index-node-expression identifier) ...) 'run . rest)
          ('for ((:= index-node-expression identifier) ...) 'run . rest))
        (('for ((:= index-node-expression identifier) ...) ((:= index-node-expression (? (lambda (e) (equal? e '(meta 0))) :_))) . rest)
          ('for ((:= index-node-expression identifier) ...) ((:= index-node-expression (? (lambda (e) (equal? e '(meta 0))) :_))) . rest))
        (((:= index-node-expression identifier) ...) ((:= index-node-expression identifier) ...))
        (else else)
        (identifier identifier) (identifier identifier) (identifier identifier)
        (identifier identifier) (identifier identifier) (identifier identifier)
        (identifier identifier) ('() '()))
     (map (lambda (l) (map (lambda (p) (annotation-stripped (index-node-datum/annotations p))) l))
      (car (generate-pair:callee+expanded identifier-reference target-index-node document)))))
(test-end)

; (test-begin "expand:step-by-step & generate-pair:template+callee & generate-pair:template+expanded for syntax-case")
;   (let* ([workspace-instance (init-workspace (current-directory))]
;       [root-file-node (workspace-file-node workspace-instance)]
;       [target-file-node (walk-file root-file-node (string-append (current-directory) "/analysis/identifier/rules/begin.sls"))]
;       [document (file-node-document target-file-node)]
;       [target-index-node (pick-index-node-from (document-index-node-list document) (text+position->int (document-text document) 19 4))]
;       [identifier-reference (car (find-available-references-for document target-index-node 'try))]
;       [expanded-expression (car (expand:step-by-step identifier-reference target-index-node document))]
;       [template+callees (generate-pair:template+callee identifier-reference target-index-node document)]
;       [expanded-index-node 
;         (init-index-node 
;           (identifier-reference-initialization-index-node identifier-reference) 
;           (car 
;             (source-file->annotations 
;               (with-output-to-string (lambda () (pretty-print expanded-expression)))
;               (uri->path (document-uri (identifier-reference-document identifier-reference))))))]
;       [template+expanded (generate-pair:template+expanded identifier-reference expanded-index-node target-index-node document template+callees)])
;     (test-equal 
;       '((call/1cc
;         (lambda (escape)
;           (with-exception-handler
;             (lambda (c) (let ([c c]) (escape (lambda () '()))))
;             (lambda ()
;               (call-with-values
;                 (lambda ()
;                   (match expression
;                     [(_ fuzzy ...)
;                       (let* ([parent (index-node-parent index-node)]
;                             [children (index-node-children index-node)]
;                             [pre-target (map index-node-references-import-in-this-node
;                                               children)]
;                             [target `(,@pre-target
;                                         ,(index-node-references-import-in-this-node
;                                           index-node))])
;                         (append-references-into-ordered-references-for
;                           document
;                           parent
;                           (apply append target)))]
;                     [else '()]))
;                 (lambda args (escape (lambda () (apply values args))))))))))
;       expanded-expression)

;     (test-equal
;       '((try . try)
;         (body0
;           match
;           expression
;           ((_ fuzzy ...)
;             (let* ([parent (index-node-parent index-node)]
;                   [children (index-node-children index-node)]
;                   [pre-target (map index-node-references-import-in-this-node
;                                     children)]
;                   [target `(,@pre-target
;                               ,(index-node-references-import-in-this-node
;                                 index-node))])
;               (append-references-into-ordered-references-for
;                 document
;                 parent
;                 (apply append target))))
;           (else '()))
;         (except . except)
;         (condition . c)
;         (clause0 else '()))
;       (map 
;         (lambda (p)
;           `(,(car p) .
;           ,(if (index-node? (cdr p))
;             (annotation-stripped (index-node-datum/annotations (cdr p)))
;             (map (lambda (px) (annotation-stripped (index-node-datum/annotations px))) (cdr p)))))
;         template+callees))

;     (test-equal 
;       '((condition . c)
;         (body0
;           match
;           expression
;           ((_ fuzzy ...)
;             (let* ([parent (index-node-parent index-node)]
;                   [children (index-node-children index-node)]
;                   [pre-target (map index-node-references-import-in-this-node
;                                     children)]
;                   [target `(,@pre-target
;                               ,(index-node-references-import-in-this-node
;                                 index-node))])
;               (append-references-into-ordered-references-for
;                 document
;                 parent
;                 (apply append target))))
;           (else '())))
;       (map 
;         (lambda (p)
;           `(,(car p) . 
;             ,(cond 
;               [(index-node? (cdr p)) (annotation-stripped (index-node-datum/annotations (cdr p)))]
;               [(find index-node? (cdr p)) 
;                 (map 
;                   (lambda (a) (annotation-stripped (index-node-datum/annotations a)))
;                   (cdr p))]
;               [else 
;                 (map 
;                   (lambda (a) 
;                     (map 
;                       (lambda (a) (annotation-stripped (index-node-datum/annotations a)))
;                       a))
;                   (cdr p))])))
;         template+expanded))

;     (test-equal
;       '((try)
;         ((match expression
;           [(_ fuzzy ...)
;             (let* ([parent (index-node-parent index-node)]
;                   [children (index-node-children index-node)]
;                   [pre-target (map index-node-references-import-in-this-node
;                                     children)]
;                   [target `(,@pre-target
;                               ,(index-node-references-import-in-this-node
;                                 index-node))])
;               (append-references-into-ordered-references-for
;                 document
;                 parent
;                 (apply append target)))]
;           [else '()])
;         (match expression
;           [(_ fuzzy ...)
;           (let* ([parent (index-node-parent index-node)]
;                   [children (index-node-children index-node)]
;                   [pre-target (map index-node-references-import-in-this-node
;                                   children)]
;                   [target `(,@pre-target
;                             ,(index-node-references-import-in-this-node
;                                 index-node))])
;             (append-references-into-ordered-references-for
;               document
;               parent
;               (apply append target)))]
;           [else '()]))
;       (except)
;       (c c)
;       ((else '())))
;       (map (lambda (l) (map (lambda (p) (annotation-stripped (index-node-datum/annotations p))) l))
;         (car (generate-pair:callee+expanded identifier-reference target-index-node document))))
;   )
; (test-end)
(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))