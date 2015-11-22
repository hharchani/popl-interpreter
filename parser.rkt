#lang racket

(require eopl)
(require "ast.rkt")
(provide (all-defined-out))

(define parse
  (lambda (exp)
    (match exp
      [(? number?)  (number exp)]
      [(? boolean?) (boolean exp)]
      [(? symbol?)  (id-ref exp)]
      [`(ifte ,e ,e1 ,e2) (ifte (parse e) (parse e1) (parse e2))]
      [`(let ,binds ,body) (assume
                            (map
                             (lambda (li)
                               (bind (first li) (parse (second li))))
                             binds)
                            (parse body))]
      [`(letrecf ,fbinds ,body) (letrecf
                                 (map
                                  (lambda (fb)
                                    (fbind (first fb) (second fb) (parse (third fb))))
                                  fbinds)
                                 (parse body))]
      [`(fn ,formals ,body) (fn formals (parse body))]
      [`(@ ,func . ,params) (@ (parse func) (map parse params))]
      [`(assign ,id ,value) (assign id (parse value))]
      [`(seq . ,statements) (seq (map parse statements))]
      [`(,proc . ,args) (@ (parse proc) (map parse args))])))
