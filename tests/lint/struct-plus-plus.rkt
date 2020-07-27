#lang racket/base

(require racket/string)

(provide
 user
 user-username
 set-user-username
 user-password-hash
 set-user-password
 set-user-password-hash)

(struct++ user
  ([username non-empty-string?]
   [password-hash bytes?])
  #:transparent)
