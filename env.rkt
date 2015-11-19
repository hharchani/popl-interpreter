#lang racket

(require eopl)
(require compatibility/mlist)
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

;;; Closure defination
(define closure
  (lambda (formals body en)
    (if ((list-of symbol?) formals)
        (if (ast? body)
            (if (env? en)
                (mlist en formals body 'closure)
                (error 'closure "contract violation: invalid enviromment"))
            (error 'closure "contract violation: invalid ast"))
        (error 'closure "contract violation: invalid list of formals"))))


(define :m-nth
  (lambda (n)
    (lambda (ml)
      (mlist-ref ml n))))

(define mfirst  (:m-nth 0))
(define msecond (:m-nth 1))
(define mthird  (:m-nth 2))
(define mfourth (:m-nth 3))

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

;;; Closure defination ends

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
        '(+ - * / = < <= > >= not eq? !=)
    (list + - * / = < <= > >= not eq? (lambda (a b)(not (= a b))))
    (empty-env)))