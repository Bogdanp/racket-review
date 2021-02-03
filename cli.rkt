#lang at-exp racket/base

(require racket/cmdline
         racket/format
         raco/command-name
         "lint.rkt")

(define (report-problem p)
  (define-values (source line column)
    (problem-loc p))

  (displayln (~a source ":" line ":" (add1 column) ":" (problem-level p) ":" (problem-message p))))

(define filename
  (command-line
   #:program (short-program+command-name)
   #:args (filename)
   (path->complete-path filename)))

(define problems (lint filename))
(unless (null? problems)
  (for ([p (in-list (sort problems problem<?))])
    (report-problem p))
  (exit 1))
