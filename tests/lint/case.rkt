#lang racket/base

(define a 1)

(case 42
  [(1 2 3)
   (define a 1)
   (define b 2)
   (+ a b)]

  [(42)
   (define b 1)
   b])

(case 'b
  ['b 1])
