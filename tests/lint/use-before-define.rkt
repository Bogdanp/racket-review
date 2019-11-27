#lang racket/base

(define (f x)
  (define y (g x))
  y)

(define (g x)
  (add 1 x))
