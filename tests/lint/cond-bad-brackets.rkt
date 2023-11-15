#lang racket/base

(cond
  (void)
  [(void)]
  (#t 'a)
  [#f 'b]
  (0 'c)
  {1 'd}
  [else 'e])
