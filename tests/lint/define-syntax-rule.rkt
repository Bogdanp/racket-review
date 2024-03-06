#lang racket/base

(require (for-syntax racket/base))

(define-syntax-rule (define-keywords id ...)
  (begin
    (provide id ...)
    (define-syntax (id stx)
      (raise-syntax-error 'id "not allowed outside special form" stx)) ...))

(define-keywords foo bar baz)
