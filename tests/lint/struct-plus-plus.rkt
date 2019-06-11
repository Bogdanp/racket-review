#lang racket/base

(require racket/string)

(provide
 user
 set-user-username
 set-user-password
 set-user-password-hash)

(struct++ user
  ([username non-empty-string?]
   [password-hash bytes?])
  #:transparent)
