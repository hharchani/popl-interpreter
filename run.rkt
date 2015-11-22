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
(define global-env
  (lambda ()
    (extended-env
     '(+ - * / = < <= > >= not eq? !=)
     (list + - * / = < <= > >= not eq? (lambda (a b)(not (= a b))))
     (empty-env))))

(define global-store new-store)

(define run
  (lambda (expr)
    (eval-ast (parse expr) (global-env) (global-store))))

(require rackunit)

(check-equal? 
    (run '(let ((x (newref 4)) (y (newref 5))) 
            (ifte (eq? (- (getref x) (getref y)) -1) #t #f )
            ))
    #t "test1")

(check-equal?
    (run
        '(let ([xr (newref 3)])
            (seq (setref xr (+ (getref xr) 2)) 
                 (+ 1 (getref xr)))
         )
    )
    6
    "test2"
)
(check-equal?
    (run
        '(let ([xr (newref 4)])
            (seq (setref xr (+ (getref xr) 2)) 
                 (+ 1 (getref xr)))
         )
    )
    7
    "test3"
)

(run '(letrecf ([fact (n) (ifte (= n 0) 1 (* n (fact (- n 1))))]) (let ([x (newref 10)]) (seq (setref x (fact (getref x))) ()))))
