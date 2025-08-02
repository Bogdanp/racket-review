#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/pre)

;; indirection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-reviewer)

(struct reviewer
  (recur-proc
   track-error-proc
   track-warning-proc
   track-binding-proc
   track-struct-usage-proc
   push-scope-proc
   pop-scope-proc
   save-proc
   undo-proc))

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
  {track-struct-usage reviewer-track-struct-usage-proc}
  {push-scope reviewer-push-scope-proc}
  {pop-scope reviewer-pop-scope-proc}
  {save reviewer-save-proc}
  {undo reviewer-undo-proc})

(module+ private
  (provide (struct-out reviewer)))


;; syntax classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 expression)

(define-syntax-class expression
  (pattern e #:do [(recur this-syntax)]))
