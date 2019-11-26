#lang at-exp racket/base

(require racket/cmdline
         racket/format
         racket/match
         raco/command-name
         "lint.rkt")

(define current-program-name
  (make-parameter (short-program+command-name)))

(define (report-problem problem [out (current-error-port)])
  (parameterize ([current-output-port out])
    (define stx (problem-stx problem))
    (define-values (source line column)
      (cond
        [(srcloc? stx)
         (values (srcloc-source stx)
                 (srcloc-line stx)
                 (srcloc-column stx))]

        [else
         (values (syntax-source stx)
                 (syntax-line stx)
                 (syntax-column stx))]))

    (displayln (~a source ":" line ":" (add1 column) ":" (problem-level problem) ":" (problem-message problem)))))

(define filename
  (command-line
   #:program (current-program-name)
   #:args (filename)
   (path->complete-path filename)))

(define problems (lint filename))
(unless (null? problems)
  (for-each report-problem (reverse problems))
  (exit 1))
