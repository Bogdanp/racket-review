#lang info

(define version "0.0.0")
(define collection "konmari")

(define deps '("base"))
(define build-deps '("base"
                     "at-exp-lib"))

(define compile-omit-paths '("tests/lint"))
(define test-omit-paths '("tests/lint"))

(define raco-commands
  '(("konmari" konmari/cli "run the konmari utility" #f)))
