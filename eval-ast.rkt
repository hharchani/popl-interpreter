#lang racket

(require eopl)
(require "ast.rkt")
(require "env.rkt")
(require "parser.rkt")
(provide (all-defined-out))

(define eval-ast
  (lambda (a e)
    (cases ast a
      [number (n) n]
      [boolean (b) b]
      [id-ref (x)
              (let ([d (lookup-env e x)])
                (denotable->expressible d))]
      [ifte (test true false)
              (let ([b (eval-ast test e)])
                (if b
                    (eval-ast true  e)
                    (eval-ast false e)))]
      [assume (binds body)
              (let*
                  ([symbols (map second binds)]
                   [expressions (map third binds)]
                   [expressible-values (map (curry-eval-ast e) expressions)]
                   [denotable-values (map expressible->denotable expressible-values)]
                   [new-env (extended-env symbols denotable-values e)])
                (eval-ast body new-env))]
      [letrecf (fbinds body)
               (let*
                   ([list-of-dummy-env (map (lambda _ (empty-env)) fbinds)]
                    [list-of-closures (map (lambda (fb en) (closure (third fb) (fourth fb) en)) fbinds list-of-dummy-env)]
                    [new-env (extended-env (map second fbinds) list-of-closures e)]
                    [d (for-each (:update-env-in-closure new-env) list-of-closures)])
                 (eval-ast body new-env))]
      [fn (formals body) (closure formals body e)]
      [@ (expr params)
         (let ([fn-to-call (eval-ast expr e)])
           (if (proc? fn-to-call)
               (cond
                 [(primitive-op? fn-to-call) (apply fn-to-call (map (curry-eval-ast e) params))]
                 [(closure? fn-to-call)
                  (let*
                      ([c fn-to-call]
                       [values (map (curry-eval-ast e) params)]
                       [list-of-formals (msecond c)]
                       [env-sitting-in-closure (mfirst c)]
                       [new-env (extended-env list-of-formals values env-sitting-in-closure)]
                       [body (mthird c)])
                    (eval-ast body new-env))])
               (error '@ "not a procedure;")))])))

(define :update-env-in-closure
  (lambda (en)
    (lambda (c)
      (update-env-in-closure c en))))

(define curry-eval-ast
  (lambda (e)
    (lambda (a)
      (eval-ast a e))))