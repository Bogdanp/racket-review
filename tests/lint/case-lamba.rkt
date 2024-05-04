#lang racket/base

(define g
  (case-lambda
    [(x) (g x 1)]
    [(x y) (f (+ x y))]))

(define (f a)
  a)
