#lang racket
(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(require "eval-ast.rkt")
(provide (all-defined-out))
(require compatibility/mlist)

;;; Global environment
(define global-env
  (extended-env
        '(+ - * / = < <= > >= not eq? !=)
    (list + - * / = < <= > >= not eq? (lambda (a b)(not (= a b))))
    (empty-env)))


(define run
  (lambda (expr)
    (eval-ast
      (parse expr) global-env)))
