#lang racket/base

(when #t
  (define-values (a b)
    (values 1 2))
  (println (list a b)))

(unless #f
  (define-values (c d)
    (values 3 4))
  (println (list c d)))

(define a 42)
(define b 43)
(define c 44)
(define d 45)
(list a b c d)
