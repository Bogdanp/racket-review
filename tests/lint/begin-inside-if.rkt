#lang racket/base

(if #t
    #f
    (let ()
      1))

(if #t
    (begin #f)
    #t)
