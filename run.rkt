#lang racket

(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(require "eval-ast.rkt")
(provide (all-defined-out))

(require compatibility/mlist)

(define run
  (lambda (expr)
    (eval-ast
      (parse expr) global-env)))
