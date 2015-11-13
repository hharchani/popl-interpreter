#lang racket

(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(require "eval-ast.rkt")

(provide (all-defined-out))

(define run
  (lambda (expr)
    (eval-ast
      (parse expr) global-env)))
