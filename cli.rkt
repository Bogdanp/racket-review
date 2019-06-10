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

(define (exit-with-errors! . messages)
  (parameterize ([current-output-port (current-error-port)])
    (for-each displayln messages))
  (exit 1))

(define (handle-help)
  (exit-with-errors!
   "usage: raco konmari <command> <option> ... <arg> ..."
   ""
   "available commands:"
   "  help   display this help message"
   "  lint   lint a Racket source file"))

(define (handle-lint)
  (define filename
    (command-line
     #:program (current-program-name)
     #:args (filename)
     (path->complete-path filename)))

  (define problems (lint filename))
  (unless (null? problems)
    (for-each report-problem problems)
    (exit 1)))

(define ((handle-unknown command))
  (exit-with-errors! @~a{error: unrecognized command '@command'}))

(define all-commands
  (hasheq 'lint  handle-lint
          'help  handle-help))

(define-values (command handler args)
  (match (current-command-line-arguments)
    [(vector command args ...)
     (values command (hash-ref all-commands (string->symbol command) (handle-unknown command)) args)]

    [_
     (values "help" handle-help null)]))

(parameterize ([current-command-line-arguments (list->vector args)]
               [current-program-name (~a (current-program-name) " " command)])
  (handler))
