# reconstruct-template

[![Build Status](https://travis-ci.org/AlexKnauth/reconstruct-template.png?branch=master)](https://travis-ci.org/AlexKnauth/reconstruct-template)

[_Documentation_](http://docs.racket-lang.org/reconstruct-template-list-ctx/index.html).

```racket
(require reconstruct-template/list-ctx)
```

Provides `~list/ctx` and `?list/ctx`, as a pattern-expander and a template-metafunction which go together so that if `x` is a syntax-list,

```racket
(syntax-parse x
  [{~list/ctx ctx e ...}
   #'{?list/ctx ctx e ...}])
=
x
```

You can think of these as similar to `(e ...)`, where `ctx` saves the lexical context, source location, and syntax properties of the parens, and transfers them over to the result syntax object.

The main intended use of `~list/ctx` and `?list/ctx` is for reconstructing syntax objects when making expanders for new *core-form languages*. This purpose is similar to [`syntax/loc/props`][syntax/loc/props] from Alexis King's blog post [_Reimplementing Hackett’s type language: expanding to custom core forms in Racket_][lexi-lambda-core-forms].

The main advantage of `~list/ctx` and `?list/ctx` over [`syntax/loc`][syntax/loc] or [`syntax/loc/props`][syntax/loc/props] is that they work even for stx-list templates nested deeply within a template, as well as for stx-list templates under ellipses. For example, if `x` is a well-formed `let` expression,

```racket
(syntax-parse x
  [{~list/ctx p1 l
              {~list/ctx p2 {~list/ctx p3 x a} ...}
              b}
   #'{?list/ctx p1 l
                {?list/ctx p2 {?list/ctx p3 x a} ...}
                b}])
=
x
```

  [syntax/loc]: https://docs.racket-lang.org/reference/stx-patterns.html#(form._((lib._racket%2Fprivate%2Fstxcase-scheme..rkt)._syntax%2Floc))

  [syntax/loc/props]: https://lexi-lambda.github.io/blog/2018/04/15/reimplementing-hackett-s-type-language-expanding-to-custom-core-forms-in-racket/#preserving-syntax-properties-and-source-locations

  [lexi-lambda-core-forms]: https://lexi-lambda.github.io/blog/2018/04/15/reimplementing-hackett-s-type-language-expanding-to-custom-core-forms-in-racket/
