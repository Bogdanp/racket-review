#lang racket/base

(require rackunit)

(define expected 42)
(define bar 1)

(test-suite
 "a"

 (test-case "b"
   (define foo 42)
   (check-equal? foo expected))

 (test-case "c"
   (define foo 43)
   (check-not-equal? foo expected)))
