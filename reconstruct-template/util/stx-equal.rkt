#lang racket/base

(provide stx=?
         stx-scopes=?
         stx-srcloc=?
         stx-props=?)

;; Any Any -> Bool
;; Checks that scopes, source-locations, and properties
;; are all exactly the same.
(define (stx=? a b)
  (cond
    [(and (identifier? a) (identifier? b))
     (and (bound-identifier=? a b)
          (stx-srcloc=? a b)
          (stx-props=? a b))]
    [(and (syntax? a) (syntax? b))
     (and (stx-scopes=? a b)
          (stx-srcloc=? a b)
          (stx-props=? a b)
          (stx=? (syntax-e a) (syntax-e b)))]
    [else
     (equal?/recur a b stx=?)]))

;; Syntax Syntax -> Bool
(define (stx-scopes=? a b)
  (bound-identifier=? (datum->syntax a '||) (datum->syntax b '||)))

;; Syntax Syntax -> Bool
(define (stx-srcloc=? a b)
  (and (equal? (syntax-source a) (syntax-source b))
       (equal? (syntax-line a) (syntax-line b))
       (equal? (syntax-column a) (syntax-column b))
       (equal? (syntax-position a) (syntax-position b))
       (equal? (syntax-span a) (syntax-span b))))

;; Syntax Syntax -> Bool
(define (stx-props=? a b)
  (define ks (syntax-property-symbol-keys a))
  (and (equal? ks (syntax-property-symbol-keys b))
       (for/and ([k (in-list ks)])
         (stx=? (syntax-property a k) (syntax-property b k)))))

