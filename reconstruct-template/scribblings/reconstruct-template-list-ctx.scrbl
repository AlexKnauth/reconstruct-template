#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     syntax/parse
                     reconstruct-template/list-ctx))

@title{Reconstruct-Template}

@(define lexi-lambda-core-forms-url
   "https://lexi-lambda.github.io/blog/2018/04/15/reimplementing-hackett-s-type-language-expanding-to-custom-core-forms-in-racket/")

@(define syntax/loc/props-url
   (string-append
    lexi-lambda-core-forms-url
    "#preserving-syntax-properties-and-source-locations"))

@(define (make-ev)
   (define ev (make-base-eval))
   (ev '(require racket
                 syntax/parse
                 reconstruct-template/list-ctx))
   ev)

@defmodule[reconstruct-template/list-ctx]{

Provides @racket[~list/ctx] and @racket[?list/ctx]
as a pattern-expander and a template-metafunction.
These two go together so that if @racket[x] is a
syntax-list:

@racketblock[
(syntax-parse x
  [{~list/ctx ctx e ...}
   #'{?list/ctx ctx e ...}])
=
x
]

You can think of these as similar to
@racket[(e ...)] as a syntax-pattern or
syntax-template, where @racket[ctx] saves the
lexical context, source location, and syntax
properties of the parens, and transfers them over
to the result syntax object.
}

@defform[{~list/ctx ctx-id pattern ...}]{
  A @racket[syntax-parse] pattern that matches a
  syntax-list like @racket[(pattern ...)]. However,
  it also binds the @racket[ctx-id] as a pattern
  variable that saves the lexical-context
  information, source-location, and
  syntax-properties of the parentheses.

  @examples[#:eval (make-ev)
    (syntax-parse #'(1 2 3)
      [{~list/ctx p a b c}
       (values #'a #'b #'c)])
    (syntax-parse #'[1 2 3]
      [{~list/ctx p a b c}
       (values (syntax-column #'p)
               (syntax-span #'p)
               (syntax-property #'p 'paren-shape))])
  ]
}

@defform[{?list/ctx ctx-id template ...}]{
  A @racket[syntax] template form that constructs a
  syntax-list like @racket[(template ...)]. However,
  it attaches the information saved in the
  @racket[ctx-id] onto the parentheses of the new
  syntax object, including its lexical-context,
  source-location, and syntax properties.

  @examples[#:eval (make-ev)
    (define stx1 #'[1 2 3])
    (define stx2
      (syntax-parse stx1
        [{~list/ctx p a b c}
         #'{?list/ctx p c b a}]))
    stx1
    stx2
    (code:comment "same source location")
    (equal? (syntax-position stx1)
            (syntax-position stx2))
    (code:comment "same syntax properties")
    (equal? (syntax-property stx1 'paren-shape)
            (syntax-property stx2 'paren-shape))
    (code:comment "same scopes")
    (bound-identifier=? (datum->syntax stx1 'x)
                        (datum->syntax stx2 'x))
  ]
}

The main intended use of @racket[~list/ctx] and
@racket[?list/ctx] is for reconstructing syntax
objects when making expanders for new core-form
languages. This purpose is similar to
@hyperlink[syntax/loc/props-url]{@racket[syntax/loc/props]}
from Alexis King's blog post
@hyperlink[lexi-lambda-core-forms-url]{
  @italic{Reimplementing Hackett's type language:
  expanding to custom core forms in Racket}}.

The main advantage of @racket[~list/ctx] and
@racket[?list/ctx] over @racket[syntax/loc] or
@hyperlink[syntax/loc/props-url]{@racket[syntax/loc/props]}
is that they work even for stx-list templates
nested deeply within a template, as well as for
stx-list templates under ellipses. For example, if
@racket[x] is a well-formed @racket[let] expression:

@racketblock[
(syntax-parse x
  [{~list/ctx p1 l
              {~list/ctx p2 {~list/ctx p3 x a} ...}
              b}
   #'{?list/ctx p1 l
                {?list/ctx p2 {?list/ctx p3 x a} ...}
                b}])
=
x
]

