#lang racket

(require syntax/parse
         syntax/parse/experimental/template
         "list-ctx.rkt"
         "or-wch.rkt")

;; An exp is one of:
;;  - x:id
;;  - n:number
;;  - (λ (x:id) b:exp)
;;  - (app f:exp a:exp)

(define (eight stx)
  (define-syntax-class exp
    [pattern
        {~or/wch w1
                 x:id
                 n:number
                 {~list/ctx p1 λ {~list/ctx p2 x} b:exp}
                 {~list/ctx p3 app f:exp a:exp}}
      #:with norm
      #'{?or/wch w1
                 (VAR x)
                 n
                 {?list/ctx p1 λ {?list/ctx p2 x} b.norm}
                 {?list/ctx p3 app f.norm a.norm}}])
  (syntax-parse stx
    [e:exp #'e.norm]))

(eight #'(app f x)) ;=> (app (VAR f) (VAR x))
