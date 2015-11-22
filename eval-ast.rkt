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
      [id-ref (x) (let ([l (lookup-env e x)])
                    (get-ref l s))]
      [ifte (test true false)
              (let ([b (eval-ast test e s)])
                (if b
                    (eval-ast true  e s)
                    (eval-ast false e s)))]
      [assume (binds body)
              (let*
                  ([symbols (map second binds)]
                   
                   [expressions (map third binds)]
                   [values-of-expressions (map (:eval-ast e s) expressions)]
                   [locations (map (curryr make-new-ref s) values-of-expressions)]
                   [new-env (extended-env symbols locations e)])
                (eval-ast body new-env s))]
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

(define :eval-ast (curryr eval-ast))
