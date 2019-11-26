#lang racket/base

(require racket/string)

(provide
 user?
 user
 user-username
 set-user-username!
 user-password
 set-user-password!
 user-password-hash
 set-user-password-hash!)

(struct user
  (username [password-hash #:mutable])
  #:transparent)
