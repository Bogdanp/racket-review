#lang info

(define license 'BSD-3-Clause)
(define version "0.2")
(define collection "review")

(define deps '("base"))
(define build-deps '("base"
                     "at-exp-lib"))

(define compile-omit-paths '("tests/lint"))
(define test-omit-paths 'all)

(define raco-commands
  '(("review" review/cli "run the review utility" #f)))
