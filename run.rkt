#lang racket
(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(require "store.rkt")
(require "eval-ast.rkt")
(provide (all-defined-out))
(require compatibility/mlist)

;;; Global environment
(define global-store new-store)

(define global-env
  (lambda (s)
    (extended-env
     '(+ - * / = < <= > >= not eq? !=)
     (map (curryr make-new-ref s) (list + - * / = < <= > >= not eq? (lambda (a b)(not (= a b)))))
     (empty-env))))

(define run/k
  (lambda (expr k)
    (let* ([gs (global-store)]
           [ge (global-env gs)]
           [parsed-expr (parser expr)])
      (eval-ast/k parsed-expr  ge gs (lambda (v) (k v))))))

(define top-k (lambda (x) x))

(define run
  (lambda (expr)
    (run/k expr top-k)))
