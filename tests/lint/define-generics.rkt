#lang racket/base

(require racket/generic)

(provide
 gen:to-jsexpr
 ->jsexpr)

(define-generics to-jsexpr
  (->jsexpr to-jsexpr)
  (->jsexpr/options wat to-jsexpr)
  (->jsexpr/bad)
  #:fast-defaults
  ())
