#lang racket
(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(require "store.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(define eval-ast/k
  (lambda (a e s k)
    (cases ast a
      [number (n) (k n)]
      [boolean (b) (k b)]
      [id-ref (x) (let ([l (lookup-env e x)])
                    (k (get-ref l s)))]
      [ifte (test true false)
            (eval-ast/k test e s
                        (lambda (ans-test)
                          (if ans-test
                              (eval-ast/k true e s (lambda (ans) (k ans)))
                              (eval-ast/k false e s (lambda (ans) (k ans))))))]
      [assume (binds body)
              (let*
                  ([symbols (map second binds)]
                   [expressions (map third binds)])
                (map/k (:eval-ast/k e s) expressions
                       (lambda (values-of-expressions)
                         (map/k (:make-new-ref/k s) values-of-expressions
                                (lambda (locations)
                                  (let ([new-env (extended-env symbols locations e)])
                                    (eval-ast/k body new-env s (lambda (v) (k v)))))))))]
      [letrecf (fbinds body)
               (let* ([list-of-empty-env (map (lambda (x) (empty-env)) fbinds)]
                      [list-of-closures (map (lambda (fb en) (closure (third fb) (fourth fb) en)) fbinds list-of-empty-env)]
                      [list-of-locations (map (curryr make-new-ref s) list-of-closures)]
                      [new-env (extended-env (map second fbinds) list-of-locations e)]
                      [d (for-each (curryr update-env-in-closure new-env) list-of-closures)])
                 (eval-ast body new-env s))]
      [fn (formals body) (closure formals body e)]
      [@ (expr params)
         (let ([fn-to-call (eval-ast expr e s)])
           (if (proc? fn-to-call)
               (cond
                 [(primitive-op? fn-to-call) (apply fn-to-call (map (:eval-ast e s) params))]
                 [(closure? fn-to-call)
                  (let*
                      ([c fn-to-call]
                       [values (map (:eval-ast e s) params)]
                       [list-of-formals (msecond c)]
                       [env-sitting-in-closure (mfirst c)]
                       [locations (map (curryr make-new-ref s) values)]
                       [new-env (extended-env list-of-formals locations env-sitting-in-closure)]
                       [body (mthird c)])
                    (eval-ast body new-env s))])
               (error '@ "not a procedure; ~a" expr)))]
      [assign (x value) (let ([l (lookup-env e x)][v (eval-ast value e s)]) (set-ref l v s) (void))]
      [seq (statements) (let ([list-of-ans (map (:eval-ast e s) statements)]) (last list-of-ans))])))

(define :eval-ast/k
  (lambda (e s)
    (lambda (a k)
      (eval-ast/k a e s (lambda (v) (k v))))))

(define :make-new-ref/k
  (lambda (s)
    (lambda (value k)
      (make-new-ref/k value s k))))