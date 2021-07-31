#lang racket/base

(define foos
  (for/list ([i (in-range 10)]
             [i (in-range 20)])
    (define foo (* i 2))
    foo))

(define foo 42)
(define bar x)

(for/list ())
(for/list ((a '(1 2 3))) a)

(for/list ([x (in-range 10)] #:when y)
  (void))

(for/list ([x (in-range 10)] #:when y)
  x)

(for/lists (a b c)
           ([x (in-range 10)]
            [y (in-range 10)]
            [z (in-range 10)])
  (values
   (cons x a)
   (cons y b)
   (cons x a)))

(for/lists (a b c #:result c)
           ([x (in-range 10)]
            [y (in-range 10)]
            [z (in-range 10)])
  #:break z
  (values
   (cons x a)
   (cons y b)
   (cons x a)))

(for/fold ([xs null])
          ([x (in-list 10)])
  #t)

(for/fold ([xs null] #:result xs)
          ([x (in-list 10)])
  #t)

(for/fold ([xs null])
          ([x (in-list 10)])
  (cons x xs))
