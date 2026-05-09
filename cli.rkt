#lang at-exp racket/base

(require racket/cmdline
         racket/runtime-path
         raco/command-name)

(define-runtime-module-path review/lint review/lint)
(define-runtime-module-path review/problem review/problem)

(define filenames
  (command-line
   #:program (short-program+command-name)
   #:args (filename . filenames)
   (map path->complete-path (cons filename filenames))))

(define ok?s
  (for/list ([filename (in-list filenames)])
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (define lint (dynamic-require review/lint 'lint))
      (define report (dynamic-require review/problem 'report))
      (define problems (lint filename))
      (unless (null? problems)
        (report problems))
      (null? problems))))
(unless (andmap values ok?s)
  (exit 1))
