#lang racket/base

(define a 42)
(define a 43)
(define (a x) x)
(define ((a x) y) (+ x y))
(define (((a x) y) z) (+ x y z))
