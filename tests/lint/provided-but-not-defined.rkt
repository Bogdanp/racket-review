#lang racket/base

(provide
 g

 (contract-out
  [a any/c]
  [y exact-integer?])

 (rename-out [h i]
             [j k]
             [l 1])
 x)

(define x 42)
(define y 24)
