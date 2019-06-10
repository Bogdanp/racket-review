#lang racket/base

(require racket/contract
         racket/file
         racket/function
         racket/port
         syntax/modread
         syntax/parse)

(provide
 (struct-out problem)
 lint)

(define current-problem-list
  (make-parameter null))

(struct problem (stx level message solution)
  #:transparent)

(define/contract (lint filename)
  (-> path-string? (listof problem?))

  (cond
    [(file->syntax filename)
     => (lambda (stx)
          (syntax-parse stx
            [module:mod
             #'module.name]

            [e
             (track-problem! stx "missing module (#lang) declaration")
             #'e]))])

  (current-problem-list))

(define (track-problem! stx message
                        #:level [level 'warning]
                        #:solution [solution identity])
  (current-problem-list
   (cons
    (problem stx level message solution)
    (current-problem-list))))

(define (file->syntax filename)
  (with-handlers ([exn:fail:read?
                   (lambda (e)
                     (begin0 #f
                       (for ([loc (exn:fail:read-srclocs e)])
                         (track-problem! loc (exn-message e) #:level 'error))))])
    (define-values (base _ __) (split-path filename))
    (parameterize ([current-load-relative-directory base]
                   [current-namespace (make-base-namespace)])
      (with-module-reading-parameterization
        (lambda ()
          (call-with-input-file filename
            (lambda (in)
              (port-count-lines! in)
              (read-syntax filename in))))))))

(define-syntax-class toplevel-expr
  (pattern e:expr))

(define-syntax-class mod
  #:datum-literals (module)
  (pattern (module name:id language:id e:toplevel-expr ...)))
