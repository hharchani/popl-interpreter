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
                    [list-of-closures (map (lambda (fb en) (closure (second fb) (third fb) en) fbinds list-of-dummy-env))]
                    [new-env (extended-env (map first fbinds) list-of-closures e)])
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
                       [list-of-formals (second c)]
                       [env-sitting-in-closure (fourth c)]
                       [new-env (extended-env list-of-formals values env-sitting-in-closure)]
                       [body (third c)])
                    (eval-ast body new-env))])
               (error '@ "not a procedure;")))])))

(define curry-eval-ast
  (lambda (e)
    (lambda (a)
      (eval-ast a e))))