#lang racket/base

(require racket/match)

(define/match (f x)
  [(1)
   (define x+1 (add1 x))
   x+1]
  [(2)
   (define x+1 (add1 x))
   x+1])

(f 1)
