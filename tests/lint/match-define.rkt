#lang racket/base

(require racket/match)

(struct foo (x y))
(define f (foo 1 2))
(match-define (foo a b) f)
