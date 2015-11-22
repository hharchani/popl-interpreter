#lang racket
(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(require "store.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(define eval-ast
  (lambda (a e s)
    (cases ast a
      [number (n) n]
      [boolean (b) b]
      [id-ref (x) (let ([d (lookup-env e x)])
                (denotable->expressible d))]
      [ifte (test true false)
              (let ([b (eval-ast test e s)])
                (if b
                    (eval-ast true  e s)
                    (eval-ast false e s)))]
      [assume (binds body)
              (let*
                  ([symbols (map second binds)]
                   [expressions (map third binds)]
                   [expressible-values (map (:eval-ast e s) expressions)]
                   [denotable-values (map expressible->denotable expressible-values)]
                   [new-env (extended-env symbols denotable-values e)])
                (eval-ast body new-env s))]
      [letrecf (fbinds body)
               (let* ([list-of-empty-env (map (lambda (x) (empty-env)) fbinds)]
                      [list-of-closures (map (lambda (fb en) (closure (third fb) (fourth fb) en)) fbinds list-of-empty-env)]
                      [new-env (extended-env (map second fbinds) list-of-closures e)]
                      [d (for-each (:update-env-in-closure new-env) list-of-closures)])
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
                       [new-env (extended-env list-of-formals values env-sitting-in-closure)]
                       [body (mthird c)])
                    (eval-ast body new-env s))])
               (error '@ "not a procedure; ~a" expr)))]
      [newref (value) (let ([value (eval-ast value e s)])
                        (make-new-ref (expressible->storable value) s))]
      [getref (ref) (let ([ref (eval-ast ref e s)])
                      (get-ref ref s))]
      [setref (ref value) (let* ([ref (eval-ast ref e s)]
                                 [value (eval-ast value e s)])
                            (set-ref ref (expressible->storable value) s))]
      [seq (statements) (let ([list-of-ans (map (:eval-ast e s) statements)]) (last list-of-ans))])))

(define :update-env-in-closure
  (lambda (en)
    (lambda (c)
      (update-env-in-closure c en))))

(define :eval-ast
  (lambda (e s)
    (lambda (a)
      (eval-ast a e s))))
