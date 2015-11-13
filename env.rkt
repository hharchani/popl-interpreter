#lang racket

(require eopl)
(require "ast.rkt")
(provide (all-defined-out))

(define-datatype env env?
  [empty-env]
  [extended-env
   (symbols (list-of symbol?))
   (values  (list-of denotable?))
   (outer-env env?)])

(define lookup-env
  (lambda (e x)
    (cases env e
      [empty-env () (error 'lookup-env (format "unbound id ~a" x))]
      [extended-env (syms vals outer-env)
        (let
          ([j (list-index syms x)])
            (if
              (= j -1)
              (lookup-env outer-env x)
              (list-ref vals j)))])))

(define expressible?
  (lambda (v)
    (or
      (number? v)
      (boolean? v)
      (proc? v))))

(define proc?
  (lambda (v)
    (or (primitive-op? v)
        (closure? v))))

(define primitive-op? procedure?)

(define denotable? expressible?)

(define denotable->expressible (lambda(x) x))
(define expressible->denotable (lambda(x) x))

;;; Closures defination

(define closure
  (lambda (formals body en)
    (if ((list-of symbol?) formals)
        (if (ast? body)
            (if (env? en)
                (list 'closure formals body en)
                (error 'closure "contract violation: invalid enviromment"))
            (error 'closure "contract violation: invalid ast"))
        (error 'closure "contract violation: invalid list of formals"))))

(define closure?
  (lambda (c)
    (and (= (length c) 4)
         (eq? (first c) 'closure)
         ((list-of symbol?) (second c))
         (ast? (third c))
         (env? (fourth c)))))

(define list-index
  (lambda (l v)
    (letrec ([find (lambda (l v i)
                     (if (empty? l)
                         -1
                         (if (eq? (first l) v)
                             i
                             (find (rest l) v (add1 i)))))])
      (if (empty? l)
          -1
          (find l v 0)))))

(define global-env
  (extended-env
        '(+ - * / = < <= > >= not !=)
    (list + - * / = < <= > >= not (lambda (a b)(not (= a b))))
    (empty-env)))