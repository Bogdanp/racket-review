#lang racket/base

(provide (struct-out voice))
(struct voice (languages name sample-rate gender) #:transparent)
