#lang racket/base

(provide ~or/wch
         ?or/wch
         ␕)

(require syntax/parse
         syntax/parse/experimental/template
         "util/stx-e-restore.rkt"
         (for-syntax racket/base
                     racket/list
                     syntax/parse))
(module+ test
  (require rackunit
           "util/stx-equal.rkt"))

;; ---------------------------------------------------------

(define sym:␕ (gensym '␕))
(define (sym:␕? v) (eq? v sym:␕))
(define-template-metafunction ␕
  (syntax-parser
    [(_) #`#,sym:␕]))

;; ~or/wch and ?or/wch go together so that
;; (syntax-parse x
;;   [{~or/wch wch v ...}
;;    #'{?or/wch wch {~? v (␕)} ...}])
;; =
;; x
;; if x matches any of the v patterns and each pair of v
;; pattern and v template cooperate.

(define-syntax ~or/wch
  (pattern-expander
   (syntax-parser
     [(_ wch v ...)
      #:with [i ...] (range (length (attribute v)))
      #'{~or* {~and v {~bind [wch (quote-syntax i)]}} ...}])))

(define-template-metafunction ?or/wch
  (λ (stx)
    (syntax-parse stx
      [(_ wch v ...)
       (define i (syntax-e #'wch))
       (define vi (list-ref (attribute v) i))
       (when (sym:␕? (syntax-e vi))
         (raise-syntax-error #f
           (format "some attribute in variant ~a is undefined" i)
           stx))
       vi])))

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
  (define-check (check-or/wch stx f)
    (check-stx=? (f stx) stx))

  (check-or/wch #'2
                (syntax-parser
                  [{~or/wch w1 n:number s:str}
                   #'{?or/wch w1 {~? n (␕)} {~? s (␕)}}]))

  (check-or/wch #'"anno"
                (syntax-parser
                  [{~or/wch w1 n:number s:str}
                   #'{?or/wch w1 {~? n (␕)} {~? s (␕)}}]))

  (check-exn #rx"\\?or/wch: some attribute in variant 0 is undefined"
             (λ ()
               (syntax-parse #'13
                 [{~or/wch w1 n:number s:str}
                  #'{?or/wch w1 {~? s (␕)} {~? s (␕)}}])))
  )

