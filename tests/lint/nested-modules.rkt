#lang racket/base

(module+ test
  (provide a)

  (define b 1)
  (define b 1)

  (if 1
      (let ()
        2)
      (begin
        3)))
