#lang racket
(require eopl)
(require compatibility/mlist)
(require "ast.rkt")
(require "store.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;; Environment
(define-datatype env env?
  [empty-env]
  [extended-env
   (symbols (list-of symbol?))
   (values  (list-of denotable?))
   (outer-env env?)])

(define lookup-env
  (lambda (e x)
    (cases env e
      [empty-env () (error 'undefined (format "unbound variable ~a" x))]
      [extended-env (syms vals outer-env)
        (let ([j (list-index syms x)])
          (if (= j -1)
              (lookup-env outer-env x)
              (list-ref vals j)))])))

;;; Closures
(define closure
  (lambda (formals body en)
    (if ((list-of symbol?) formals)
        (if (ast? body)
            (if (env? en)
                (mlist en formals body 'closure)
                (error 'closure "contract violation: invalid enviromment"))
            (error 'closure "contract violation: invalid ast"))
        (error 'closure "contract violation: invalid list of formals"))))

(define closure?
  (lambda (c)
    (and (mlist? c)
         (= (mlength c) 4)
         (eq? (mfourth c) 'closure)
         ((list-of symbol?) (msecond c))
         (ast? (mthird c))
         (env? (mfirst c)))))

(define update-env-in-closure
  (lambda (c en)
    (set-mcar! c en)))

;;; Value types

(define primitive-op? procedure?)

(define proc?
  (lambda (v)
    (or (primitive-op? v)
        (closure? v))))

(define expressible?
  (lambda (v)
    (or
      (number? v)
      (boolean? v)
      (proc? v)
      (void? v))))
(define storable? expressible?)
(define denotable? ref?)
