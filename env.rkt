#lang racket
(require eopl)
(require compatibility/mlist)
(require "ast.rkt")
(require "utilities.rkt")
(require "top.rkt")
(provide (all-defined-out))

;;; Environment
(define-datatype env env?
  [empty-env]
  [extended-env
   (symbols (list-of symbol?))
   (values  (list-of denotable?))
   (outer-env env?)])

(define lookup-env/k
  (lambda (e x k)
    (cases env e
      [empty-env () (error 'undefined (format "unbound variable ~a" x))]
      [extended-env (syms vals outer-env)
        (let ([j (list-index syms x)])
          (if (= j -1)
              (lookup-env/k outer-env x (lambda (v) (apply-k k v)))
              (apply-k k (list-ref vals j))))])))

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

(define denotable? expressible?)
