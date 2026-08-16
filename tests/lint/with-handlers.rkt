#lang racket/base

(define (foo x)
  (define y x)
  (with-handlers ([exn:fail? raise])
    (define y (* x 2))
    y))
