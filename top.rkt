#lang racket
(require eopl)
(provide (all-defined-out))

(define top-k
  (lambda ()
    (lambda (x) x)))

(define apply-k
  (lambda (k val)
    (k val)))
