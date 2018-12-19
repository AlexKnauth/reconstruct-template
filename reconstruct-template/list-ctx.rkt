#lang racket/base

(provide ~list/ctx
         ?list/ctx)

(require syntax/parse
         syntax/parse/experimental/template
         "util/stx-e-restore.rkt"
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit
           "util/stx-equal.rkt"))

;; ---------------------------------------------------------

;; ~list/ctx and ?list/ctx go together so that
;; (syntax-parse x
;;   [{~list/ctx ctx e ...}
;;    #'{?list/ctx ctx e ...}])
;; =
;; x
;; if x is a syntax list.

(define-syntax ~list/ctx
  (pattern-expander
   (syntax-parser
     [(_ ctxlocprop . stuff)
      #'{~and ctxlocprop stuff}])))

(define-template-metafunction ?list/ctx
  (syntax-parser
    [(_ ctxlocprop . stuff)
     (restore #'ctxlocprop (syntax-e #'stuff))]))

;; ---------------------------------------------------------

(module+ test
  ;; Checks that scopes, source-locations, and properties
  ;; are all exactly the same.
  (define-binary-check (check-stx=? stx=? actual expected))

  ;; -------------------------------------------------------

  ;; Example:
  ;;     (check-list/ctx #'(λ (x) x)
  ;;                     (syntax-parser
  ;;                       [{~list/ctx p1 l {~list/ctx p2 x1} x2}
  ;;                        #'{?list/ctx p1 l {?list/ctx p2 x1} x2}]))
  (define-check (check-list/ctx stx f)
    (check-stx=? (f stx) stx))

  (check-list/ctx #'(λ (x) x)
                  (syntax-parser
                    [{~list/ctx p1 l {~list/ctx p2 x1} x2}
                     #'{?list/ctx p1 l {?list/ctx p2 x1} x2}]))

  (check-list/ctx #'(let ([x 1] [y 2]) (+ x y))
                  (syntax-parser
                    [{~list/ctx p1 l
                                {~list/ctx p2 {~list/ctx p3 x a} ...}
                                b}
                     #'{?list/ctx p1 l
                                  {?list/ctx p2 {?list/ctx p3 x a} ...}
                                  b}]))
  )

