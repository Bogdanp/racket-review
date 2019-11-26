#lang info

(define version "0.0.0")
(define collection "review")

(define deps '("base"))
(define build-deps '("base"
                     "at-exp-lib"))

(define compile-omit-paths '("tests/lint"))
(define test-omit-paths '("tests/lint"))

(define raco-commands
  '(("review" review/cli "run the review utility" #f)))
