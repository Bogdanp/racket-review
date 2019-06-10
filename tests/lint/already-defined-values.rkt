#lang racket/base

(define-values (a a b)
  (values 1 2 3))

(define-values (a _ _)
  (values 1 2 3))

(define-values (a _ _)
  (define-values (b b)
    (values 1 2))
  (values b b))

(define-values (x y z)
  (values 1 2 3))
