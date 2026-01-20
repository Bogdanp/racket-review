#lang racket/base

(require racket/class)

(define a-class%
  (class* object% ()
    (super-new)
    (define root #f)))

(define root 1)
