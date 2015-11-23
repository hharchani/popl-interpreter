#lang racket
(require eopl)
(require "eval-ast.rkt")
(require "parser.rkt")
(require "primitives.rkt")
(require "top.rkt")
(provide (all-defined-out))
(require compatibility/mlist)

(define run/k
  (lambda (expr k)
      (eval-ast/k (parser expr) top-env (lambda (v) (apply-k k v)))))

(define run
  (lambda (expr)
    (run/k expr (top-k))))

(require rackunit)
