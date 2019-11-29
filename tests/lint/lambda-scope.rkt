#lang racket/base

(lambda _
  (void))

(lambda _
  (void))

(lambda (a)
  a)

(lambda (a b)
  a)

(lambda (a b c)
  a)

(lambda (a b . c)
  a)

(lambda (a [b a] #:c c #:d [d #f] . xs)
  x)
