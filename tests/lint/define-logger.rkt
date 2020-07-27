#lang racket/base

(provide
 log-example-debug
 do-something-else)

(define log-example-error 1)

(define-logger example)

(log-example-info "hello!")
