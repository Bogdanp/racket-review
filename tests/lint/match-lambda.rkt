#lang racket/base

(require racket/match)

(define foo
  (match-lambda
    [#t
     (define a 1)
     (+ a 2)]))

(define bar
  (match-lambda
    [#t
     (define a 2)
     (+ a 1)]))
