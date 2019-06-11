#lang racket/base

;; This currently works only if the lambda comes after the definition.
;; We need a way to punt on bindings.

(lambda ()
  (show "hello"))

(define (show s)
  (displayln s))
