#lang racket/base

(require racket/match)

(match 10
  [x x]
  [else 'foo])
