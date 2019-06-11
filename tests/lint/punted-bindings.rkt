#lang racket/base

(provide
 (contract-out
  [a an-a?]))

(define a 'a)

(define (an-a? v)
  (eq? v 'a))

(lambda ()
  (show "hello")

  (let ()
    (define (show s)
      (displayln s))))
