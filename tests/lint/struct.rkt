#lang racket/base

(require racket/string)

(provide
 user?
 user
 user-username
 user-password
 user-password-hash)

(struct user
  (username password-hash)
  #:transparent)

(define u (user "a" ""))
(set-user-username! u "b")
