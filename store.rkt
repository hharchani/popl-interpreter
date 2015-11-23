#lang racket
(require eopl)
(provide (all-defined-out))
;;;store?  ref?  new-store  new-ref  new-refs  getref setref setrefs

(define new-store make-hash)

(define store? hash?)

(define ref?
  (lambda (r)
    (and (list? r)
         (= (length r) 2)
         (eq? (first r) 'store-ref)
         (number? (second r)))))

(define make-new-ref/k
  (lambda (value s k)
    (let ([len (hash-count s)])
      (hash-set! s (+ len 1) value)
      (k (list 'store-ref (+ len 1))))))

(define get-ref
  (lambda (ref s)
    (if (ref? ref)
        (if (hash-has-key? s (second ref))
            (hash-ref s (second ref))
            (error 'getref "Location doesn't exists"))
        (error 'getref "Invalid memory location"))))

(define set-ref
  (lambda (ref value s)
    (if (ref? ref)
        (if (hash-has-key? s (second ref))
            (hash-set! s (second ref) value)
            (error 'getref "Location doesn't exists"))
        (error 'getref "Invalid memory location"))))