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

(lambda _
  (show "hello")

  (define (show s)
    (displayln s)))


;; This is a false-positive caused by the recursion inside of function-header.
(define (breadcrumbs . crumbs)
  (haml (:ul.breadcrums (@ crumbs))))

(define (breadcrumb uri label)
  (void))

(breadcrumbs
 (breadcrumb))
