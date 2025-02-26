#lang racket/base

(provide
 display-leaf-timings)

(define (display-leaf-timings tree)
  (for*/fold ([tree (hash)]
              #:result tree)
             ([(mod-path dependencies) (in-hash tree)]
              [dep (in-list dependencies)])
    (hash-update tree dep (λ (dependents) (cons mod-path dependents)) null)))
