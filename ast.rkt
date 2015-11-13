#lang racket

(require eopl)
(provide (all-defined-out))

(define-datatype ast ast?
  [number (n number?)]
  [boolean (b boolean?)]
  [id-ref (x symbol?)]
  [ifte (test ast?)
        (true ast?)
        (false ast?)]
  [assume (binds (list-of bind?))
          (body ast?)]
  [letrecf (fbinds (list-of fbind?))
           (body ast?)]
  [fn (formals (list-of symbol?))
      (expr ast?)]
  [@ (expr ast?)
     (params (list-of ast?))])

(define bind
  (lambda (x a)
    (if (symbol? x)
        (if (ast? a)
            (list 'bind x a)
            (error 'bind "contract violation: Invalid bind expression;"))
        (error 'bind "contract violation: Invalid bind symbol;"))))

(define bind?
  (lambda (b)
    (and
      (= (length b) 3)
      (eq? (first b) 'bind)
      (symbol? (second b))
      (ast? (third b)))))

(define fbind
  (lambda (name list-of-formals body)
    (if (symbol? name)
        (if ((list-of symbol?) list-of-formals)
            (if (ast? body)
                (list 'fbind name list-of-formals body)
                (error 'fbind "contract violation: Invalid function body"))
            (error 'fbind "contract violation: Invalid list of formals"))
        (error 'fbind "contract violation: Invalid function name"))))

(define fbind?
  (lambda (fb)
    (and
     (= (length fb) 4)
     (eq? (first fb) 'fbind)
     (symbol? (second fb))
     ((list-of symbol?) (third fb))
     (ast? (fourth fb)))))
                