#lang racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  (define x 42))

(module foo racket/base
  (provide y)
  (define x 42)
  (define (f x)
    (println x)))

(module bar racket/base
  (require racket/string)
  (provide non-empty-string?))

(provide x)
