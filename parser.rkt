#lang racket
(require eopl)
(require "ast.rkt")
(provide (all-defined-out))

(define parser
  (lambda (exp)
    (match exp
      [(? number?)  (number exp)]
      [(? boolean?) (boolean exp)]
      [(? symbol?)  (id-ref exp)]
      [`(ifte ,e ,e1 ,e2) (ifte (parser e) (parser e1) (parser e2))]
      [`(let ,binds ,body) (assume
                            (map
                             (lambda (li)
                               (bind (first li) (parser (second li))))
                             binds)
                            (parser body))]
      [`(assume ,binds ,body) (assume
                            (map
                             (lambda (li)
                               (bind (first li) (parser (second li))))
                             binds)
                            (parser body))]
      [`(fn ,formals ,body) (fn formals (parser body))]
      [`(@ ,func . ,params) (@ (parser func) (map parser params))]
      [`(,proc . ,args) (@ (parser proc) (map parser args))])))
