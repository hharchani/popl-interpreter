#lang racket
(require eopl)
(provide (all-defined-out))

(define-datatype ast ast?
  [number (n number?)]
  [boolean (b boolean?)]
  [id-ref (x symbol?)]
  [ifte (test ast?)
        (true ast?)
        (false ast?)]
  [assume (binds (list-of bind?))
          (body ast?)]
  [fn (formals (list-of symbol?))
      (expr ast?)]
  [prim-app (expr ast?) (params (list-of ast?))]
  [app (expr ast?) (params (list-of ast?))]
  [@ (expr ast?) (params (list-of ast?))])

;;; Bind declarations for let

(define bind
  (lambda (x a)
    (if (symbol? x)
        (if (ast? a)
            (list 'bind x a)
            (error 'bind "contract violation: Invalid bind expression;"))
        (error 'bind "contract violation: Invalid bind symbol;"))))

(define bind?
  (lambda (b)
    (and (list? b)
         (= (length b) 3)
         (eq? (first b) 'bind)
         (symbol? (second b))
         (ast? (third b)))))
