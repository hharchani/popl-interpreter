#lang racket
(require eopl)
(require "env.rkt")
(provide (all-defined-out))

(define top-env
  (extended-env
   '(+ - * / = < <= > >= not eq? !=)
   (list + - * / = < <= > >= not eq? (lambda (a b)(not (= a b))))
   (empty-env)))
