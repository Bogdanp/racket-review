#lang racket/base

(define (a-page req)
  (let loop ([req req])
    (send/suspend/dispatch
     (lambda (embed/url)
       (displayln req)
       (embed/url loop)))))
