#lang racket/base

(require racket/generic
         racket/match)

(define-generics store
  {store-ref store})

(struct memory-store (ttl)
  #:methods gen:store
  [(define (store-ref s)
     (match-define (memory-store ttl) s)
     (void))])
