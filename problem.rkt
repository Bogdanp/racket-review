#lang racket/base

(require racket/format)

(provide
 (struct-out problem)
 problem<?
 problem-loc
 current-problem-list
 report)

(struct problem (stx level message)
  #:transparent)

(define (problem<? a b)
  (define-values (_source-a line-a column-a) (problem-loc a))
  (define-values (_source-b line-b column-b) (problem-loc b))
  (if (= line-a line-b)
      (< column-a column-b)
      (< line-a line-b)))

(define (problem-loc p)
  (define stx (problem-stx p))
  (if (srcloc? stx)
      (values (srcloc-source stx) (srcloc-line stx) (srcloc-column stx))
      (values (syntax-source stx) (syntax-line stx) (syntax-column stx))))

(define (report ps)
  (for-each report-problem (sort ps problem<?)))

(define (report-problem p)
  (define-values (source line column)
    (problem-loc p))
  (displayln (~a source ":" line ":" (add1 column) ":" (problem-level p) ":" (problem-message p))))

(define current-problem-list
  (make-parameter null))
