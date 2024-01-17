#lang racket/base

(require racket/contract/base
         racket/contract/region)

(define/contract (a x y [s 'a])
  (-> [natural-number/c natural-number/c] [(or/c 'a 'b)] void?)
  (void))

(define/contract (b a [b #f])
  (->* (natural-number/c)
       ((or/c #f number?))
       any)
  (void))
