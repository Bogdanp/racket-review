#lang racket/base

(let ([x 5]
      [x 42])
  x)

(let loop ([x 5]
           [x 42])
  x)

(define (f x)
  (let ([x 5]
        [x 10])
    x))
