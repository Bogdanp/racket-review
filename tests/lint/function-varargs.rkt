#lang racket/base

(define (f x)
  (void))

(define (f . xs)
  (void))

(define (g a b)
  (void))

(define (h a b . xs)
  (void))

(define (i x [y 1] #:z z #:w [w z] . args)
  (cons w args))
