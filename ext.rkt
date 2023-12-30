#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/pre)

(provide
 current-reviewer)

(struct reviewer
  (recur-proc
   track-error-proc
   track-warning-proc
   track-binding-proc
   push-scope-proc
   pop-scope-proc))

(define current-reviewer
  (make-parameter #f))

(define-syntax (define-reviewer-procs stx)
  (syntax-case stx ()
    [(_ {id accessor} ...)
     #'(begin
         (provide id ...)
         (define id
           (make-keyword-procedure
            (lambda (kws kw-args . args)
              (keyword-apply
               (accessor (current-reviewer))
               kws kw-args args))))
         ...)]))

(define-reviewer-procs
  {recur reviewer-recur-proc}
  {track-error reviewer-track-error-proc}
  {track-warning reviewer-track-warning-proc}
  {track-binding reviewer-track-binding-proc}
  {push-scope reviewer-push-scope-proc}
  {pop-scope reviewer-pop-scope-proc})

(provide
 define-expression-syntax-class)

(define-syntax (define-expression-syntax-class stx)
  (syntax-case stx ()
    [(_ id)
     #'(define-syntax-class id
         (pattern e #:do [(recur this-syntax)]))]))

(module+ private
  (provide (struct-out reviewer)))
