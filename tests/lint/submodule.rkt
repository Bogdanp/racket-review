#lang racket/base

(module unsafe racket/base
  (provide (all-defined-out))
  (define x 42))

(provide x)
