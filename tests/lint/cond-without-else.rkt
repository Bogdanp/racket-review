#lang racket/base

(define x 0)

(cond
  [(> x 0) 'positive])

(define res
  (cond
    [(> x 0) 'positive]))

(define (f x)
  (cond
    [(> x 0) 'positive]))
