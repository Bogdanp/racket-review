#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

(define-runtime-path linter-tests
  (build-path "tests" "lint"))

(file-stream-buffer-mode (current-output-port) 'none)
(file-stream-buffer-mode (current-error-port) 'none)

(define (indent s)
  (~a "  " s))

(define (update-output-file! filepath output)
  (with-output-to-file filepath
    #:exists 'truncate/replace
    (lambda ()
      (for-each displayln output))))

(define (strip-prefixes line)
  (string-replace line (path->string linter-tests) ""))

(for ([filename (directory-list linter-tests)]
      #:when (bytes=? (path-get-extension filename) #".rkt"))
  (define-values (in out) (make-pipe))
  (define input-filepath (build-path linter-tests filename))
  (define output-filepath (~a input-filepath ".out"))

  (match-define (list _ _ _ _ control)
    (process*/ports out #f out (find-executable-path "raco") "review" input-filepath))
  (control 'wait)

  (close-output-port out)
  (define command-output (map strip-prefixes (port->lines in)))
  (define expected-output
    (cond
      [(file-exists? output-filepath)
       (call-with-input-file output-filepath port->lines)]

      [else
       (begin0 command-output
         (update-output-file! output-filepath command-output))]))

  (unless (equal? command-output expected-output)
    (displayln (~a "output differs when linting " output-filepath))
    (displayln "expected:")
    (for-each (compose1 displayln indent) expected-output)
    (displayln "found:")
    (for-each (compose1 displayln indent) command-output)
    (display "update? ")
    (let loop ()
      (case (read-char)
        [(#\y) (update-output-file! output-filepath command-output)]
        [(#\n) (exit 1)]
        [else (loop)]))))
