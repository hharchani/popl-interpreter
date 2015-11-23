#lang racket
(require eopl)
(require compatibility/mlist)
(provide (all-defined-out))

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

(define :m-nth
  (lambda (n)
    (lambda (ml)
      (mlist-ref ml n))))

(define mfirst  (:m-nth 0))
(define msecond (:m-nth 1))
(define mthird  (:m-nth 2))
(define mfourth (:m-nth 3))

(define map/k
  (lambda (f/k ls k)
    (if (null? ls)
        (k '())
        (f/k (first ls) (lambda (v)
                          (map/k f/k (rest ls)
                                      (lambda (u)
                                        (k (cons v u)))))))))
