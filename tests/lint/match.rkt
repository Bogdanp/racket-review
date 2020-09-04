#lang racket/base

(require racket/match)

(define foo 1)

(match foo
  [1
   (define foo 2)
   (define bar 3)
   (+ foo bar)]

  [2
   (define bar 4)
   bar])

(match foo
  [1
   (define bar 3)
   bar])

(match foo
  [null 1]
  [_ 2])
