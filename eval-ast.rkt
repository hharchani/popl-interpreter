#lang racket
(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(require "utilities.rkt")
(require "top.rkt")
(provide (all-defined-out))

(define eval-ast/k
  (lambda (a e k)
    (cases ast a
      [number (n) (apply-k k n)]
      [boolean (b) (apply-k k b)]
      [id-ref (x) (lookup-env/k e x (lambda (v) (apply-k k v)))]
      [ifte (test true false)
            (eval-ast/k test e
                        (lambda (ans-test)
                          (if ans-test
                              (eval-ast/k true e (lambda (ans) (apply-k k ans)))
                              (eval-ast/k false e (lambda (ans) (apply-k k ans))))))]
      [assume (binds body)
              (let*
                  ([symbols (map second binds)]
                   [expressions (map third binds)])
                (map/k (:eval-ast/k e) expressions
                       (lambda (values-of-expressions)
                                  (let ([new-env (extended-env symbols values-of-expressions e)])
                                    (eval-ast/k body new-env (lambda (v) (apply-k k v)))))))]
      [fn (formals body) (apply-k k (closure formals body e))]
      [@ (expr params)
         (eval-ast/k expr e (lambda (fn)
                              (if (proc? fn)
                                  (cond
                                    [(primitive-op? fn)
                                     (map/k (:eval-ast/k e) params (lambda (args)
                                                                     (apply-k k (apply fn args))))]
                                    [(closure? fn)
                                     (map/k (:eval-ast/k e) params
                                            (lambda (args)
                                              (let*
                                                  ([list-of-formals (msecond fn)]
                                                   [env-sitting-in-closure (mfirst fn)]
                                                   [body (mthird fn)]
                                                   [new-env (extended-env list-of-formals args env-sitting-in-closure)])
                                                (:eval-ast/k body new-env (lambda (answer) (apply-k k answer))))))])
                                  (error '@ "not a procedure; ~a" expr))))]
      [prim-app (expr params)
                (eval-ast/k expr e
                            (lambda (fn)
                              (map/k (:eval-ast/k e) params
                                     (lambda (args)
                                       (apply-k k (apply fn args))))))]
      [app (expr params)
           (eval-ast/k expr e
                       (lambda (fn)
                         (map/k (:eval-ast/k e) params
                                            (lambda (args)
                                              (let*
                                                  ([list-of-formals (msecond fn)]
                                                   [env-sitting-in-closure (mfirst fn)]
                                                   [body (mthird fn)]
                                                   [new-env (extended-env list-of-formals args env-sitting-in-closure)])
                                                (:eval-ast/k body new-env (lambda (answer) (apply-k k answer))))))))])))

(define :eval-ast/k
  (lambda (e)
    (lambda (a k)
      (eval-ast/k a e (lambda (v) (apply-k k v))))))
