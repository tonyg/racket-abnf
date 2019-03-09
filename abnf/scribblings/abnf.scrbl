#lang scribble/manual

@require[@for-label[abnf
                    racket/base
                    racket/match]]

@title{abnf: RFC5234-compatible ABNF #lang}
@author[(author+email "Tony Garnock-Jones" "tonyg@leastfixedpoint.com")]

@define-syntax-rule[(grammar-snippet str ...) (nested #:style 'code-inset (verbatim str ...))]

@section{Introduction}

@defmodule[abnf]

This library implements a @tt{#lang} and compiler for grammars
specified in the ABNF dialect given by
@link["http://tools.ietf.org/html/rfc5234"]{RFC 5234}.

Using @tt{#lang abnf} allows grammars to be directly cut-and-pasted
from RFC documents, giving confidence that an implementation matches
the specification.

In addition, the usual advantages of parser-generator tools over
hand-implemented lexers and parsers apply: readability and
maintainability of code, and automated syntax-error handling.

The chief disadvantage of using @tt{#lang abnf} is a modest speed
penalty. Also, input must be given as either a @racket[string?] or a
@racket[bytes?]. Parsing from a port is not yet supported.

As an example, compare
@link["https://github.com/racket/racket/blob/f720220c41551af819d21c6fffe66433abc43bbb/racket/collects/json/main.rkt#L160-L459"]{Racket's
own JSON parser} to a @tt{#lang abnf} parser directly using the ABNF
from @link["https://tools.ietf.org/html/rfc8259"]{RFC 8259}. The
built-in parser is about 274 lines of hand-written code. It implements
a JSON dialect originally based on the
@link["http://json.org/"]{@tt{json.org}} specification. However, the
correspondence between the implementation and the @tt{json.org}
grammar is not clear. By contrast, directly cutting-and-pasting the
ABNF rules from RFC 8259 yields
@link["https://github.com/tonyg/racket-abnf/blob/1c2d05ae97affc90e1d5cd3924f867ad5f7ac81d/abnf/rfc8259/rules.rkt"]{48
lines of @tt{#lang abnf}} for the grammar itself, which combine with
about
@link["https://github.com/tonyg/racket-abnf/blob/1c2d05ae97affc90e1d5cd3924f867ad5f7ac81d/abnf/rfc8259.rkt#L10-L62"]{50
lines of hand-written code} to produce a compatible JSON reader that
conforms to the RFC's own grammar. At the time of writing, the
@tt{#lang abnf} parser is approximately 5–10× slower than the
hand-written parser.

@section{Theory of operation}

@margin-note*{The idea of separating specification of concrete syntax
from abstract syntax and from semantics in general was taken from the
@link["https://ohmlang.github.io/"]{Ohm} parser generator. See also
the paper
@link["https://ohmlang.github.io/pubs/dls2016/modular-semantic-actions.pdf"]{“Modular
Semantic Actions”, Warth, Dubroy and Garnock-Jones, DLS 2016}.}This
library, unlike many other parser generators, separates parsing of an
input into two separate stages.

First, a grammar given in a @tt{#lang abnf} source file translates the
input to a @emph{concrete syntax tree} (@deftech{CST}). Second, the
CST is interpreted by a @deftech{semantic function} to produce an
effect, a final value, or an @emph{abstract syntax
tree} (@deftech{AST}).

A @tech{CST} preserves almost every detail of the surface syntax of
the input as analyzed by the rules of the grammar, including
whitespace and a complete record of which grammatical rules were
applied where. By contrast, an @tech{AST} abstracts away much of the
syntactic detail of an input, representing the essential structure of
the input in terms of the language being implemented.

Separation of concrete from abstract syntax allows for reuse of
grammars in many different contexts. Indeed, one of the main
motivations for this library was to be able to directly cut and paste
ABNF specifications from RFCs, keeping a close correspondence between
a grammar and its defining document, without being required to alter
it syntactically or to insert semantic actions.

@section{Example: Arithmetic}

We will use a simple language of arithmetic to introduce the use of
the library and @tt{#lang}. We will begin by parsing the language's
concrete syntax, move on to direct interpretation from the concrete
syntax, and conclude by producing an @tech{AST} from the @tech{CST}
and then interpreting the @tech{AST}.

@subsection{Grammar and concrete syntax}

Consider the following Racket source file, @tt{arithmetic-rules.rkt}:

@grammar-snippet{
#lang abnf

expr   = term "+" expr / term "-" expr / term
term   = factor "*" term / factor "/" term / factor
factor = "(" expr ")" / num
num    = *SP *DIGIT *SP
SP     = %x20 / %x09 / %x0d / %x0a
DIGIT  = %x30-39
}

A program can produce a @tech{CST} for a given input as follows:

@racketinput[(require abnf)
             (define-abnf-parser parse-arithmetic/cst "arithmetic-rules.rkt" expr values)
             (parse-arithmetic/cst "10 + 20 * 30 - 40")]

The @tech{CST} produced for that input is the fearsome-looking S-expression:

@racketresultblock['(expr (/ 0 (: (term
                                   (/ 2 (factor
                                         (/ 1 (num (: (* ())
                                                      (* ((DIGIT 49)
                                                          (DIGIT 48)))
                                                      (* ((SP (/ 0 32))))))))))
                                  "+"
                                  (expr
                                   (/ 1 (: (term
                                            (/ 0 (: (factor
                                                     (/ 1 (num (: (* ((SP (/ 0 32))))
                                                                  (* ((DIGIT 50)
                                                                      (DIGIT 48)))
                                                                  (* ((SP (/ 0 32))))))))
                                                    "*"
                                                    (term
                                                     (/ 2 (factor
                                                           (/ 1 (num (: (* ((SP (/ 0 32))))
                                                                        (* ((DIGIT 51)
                                                                            (DIGIT 48)))
                                                                        (* ((SP (/ 0 32)))))))))))))
                                           "-"
                                           (expr
                                            (/ 2 (term
                                                  (/ 2 (factor
                                                        (/ 1 (num (: (* ((SP (/ 0 32))))
                                                                     (* ((DIGIT 52)
                                                                         (DIGIT 48)))
                                                                     (* ())))))))))))))))]

The utility @racket[text], imported via @racket[(require abnf/rfc5234/core)],
recovers the input text from a given CST:

@racketinput[(require abnf/rfc5234/core)
             (text (parse-arithmetic/cst "10 + 20 * 30 - 40"))]
@racketresultblock["10 + 20 * 30 - 40"]

@subsection{Direct interpretation}

Noting that

@itemlist[

@item{each matched rule produces @racket[`(#,(emph "rulename")
#,(emph "cst"))],}

@item{alternatives in the grammar produce @racket[`(/ #,(emph "i")
#,(emph "cst"))], where @emph{i} is the index of the matched
alternative,}

@item{sequences produce @racket[`(: #,(emph "cst" (subscript "1"))
#,(emph "cst" (subscript "2")) #,(elem "..."))],}

@item{repetitions produce @racket[`(* (#,(emph "cst" (subscript "1"))
#,(emph "cst" (subscript "2")) #,(elem "...")))], and}

@item{literal matches produce either a string (e.g. @racket["+"] on
line 7) or an integer codepoint number (e.g. @racket[49] on line 4),}

]

we can write a direct interpreter for arithmetic @tech{CSTs} as
follows:

@racketblock[(define (eval-cst cst)
               (match cst
                 [`(expr (/ 0 (: ,x "+" ,y))) (+ (eval-cst x) (eval-cst y))]
                 [`(expr (/ 1 (: ,x "-" ,y))) (- (eval-cst x) (eval-cst y))]
                 [`(expr (/ 2 ,z)) (eval-cst z)]

                 [`(term (/ 0 (: ,x "*" ,y))) (* (eval-cst x) (eval-cst y))]
                 [`(term (/ 1 (: ,x "/" ,y))) (/ (eval-cst x) (eval-cst y))]
                 [`(term (/ 2 ,z)) (eval-cst z)]

                 [`(factor (/ 0 (: "(" ,z ")"))) (eval-cst z)]
                 [`(factor (/ 1 ,z)) (eval-cst z)]
                 [`(num (: ,_ ,digits ,_)) (string->number (text digits))]))]

The example above evaluates as expected:

@racketinput[(eval-cst (parse-arithmetic/cst "10 + 20 * 30 - 40"))]
@racketresultblock[570]

@subsection{Building and interpreting an @tech{AST}}

Many languages are too complex to interpret directly from concrete
syntax. A useful intermediate step is to define an abstract syntax and
to translate input concrete syntax to abstract syntax before
processing it further.

For our simple arithmetic language, a single syntax tree node type
suffices:

@racketinput[(struct binop (function arg0 arg1) #:prefab)]

Our semantic function is almost identical to the evaluator above:

@racketblock[(define (cst->ast cst)
               (match cst
                 [`(expr (/ 0 (: ,x "+" ,y))) (binop + (cst->ast x) (cst->ast y))]
                 [`(expr (/ 1 (: ,x "-" ,y))) (binop - (cst->ast x) (cst->ast y))]
                 [`(expr (/ 2 ,z)) (cst->ast z)]

                 [`(term (/ 0 (: ,x "*" ,y))) (binop * (cst->ast x) (cst->ast y))]
                 [`(term (/ 1 (: ,x "/" ,y))) (binop / (cst->ast x) (cst->ast y))]
                 [`(term (/ 2 ,z)) (cst->ast z)]

                 [`(factor (/ 0 (: "(" ,z ")"))) (cst->ast z)]
                 [`(factor (/ 1 ,z)) (cst->ast z)]
                 [`(num (: ,_ ,digits ,_)) (string->number (text digits))]))]

It produces abstract syntax trees that capture the essence of the
input expression without details of its syntactic presentation such as
whitespace or grouping parentheses:

@racketinput[(cst->ast (parse-arithmetic/cst "10 + 20 * 30 - 40"))]
@racketresultblock[(binop + 10 (binop - (binop * 20 30) 40))]

We can then write an interpreter for our abstract syntax:

@racketblock[(define (eval-ast ast)
               (match ast
                 [(binop f x y) (f (eval-ast x) (eval-ast y))]
                 [(? number? n) n]))]

The interpreter produces the same results as the @tech{CST}
interpreter:

@racketinput[(eval-ast (cst->ast (parse-arithmetic/cst "10 + 20 * 30 - 40")))]
@racketresultblock[570]

The combination of a semantic function with a concrete grammar is so
common that it is included in the syntax of
@racket[define-abnf-parser]. We can define an @tech{AST}-producing
variant parser by supplying @racket[cst->ast] instead of
@racket[values] to @racket[define-abnf-parser]:

@racketblock[(define-abnf-parser parse-arithmetic "arithmetic-rules.rkt" expr cst->ast)]

Then @racket[parse-arithmetic] produces @tech{AST} values directly:

@racketinput[(parse-arithmetic "10 + 20 * 30 - 40")]
@racketresultblock[(binop + 10 (binop - (binop * 20 30) 40))]

@section{@tt{#lang abnf} source files}

abnf per rfc, plus // and # and @racket[\@require] and @racket[\@biased-choice] and @racket[\@unbiased-choice] and @racket[\@greedy-repetition] and @racket[\@non-greedy-repetition]

how imports and exports work

writing compatible parsers by hand (forward reference to parser inputs and outputs)

@section{Parser inputs and outputs}

@declare-exporting[abnf]

parser function type

->parse-input

parse-input?

@defproc[(cst? [maybe-cst any]) boolean?]{
x
}

convert-all-results

exceptions:
         (struct-out exn:fail:abnf)
         (struct-out exn:fail:abnf:syntax)
         (struct-out exn:fail:abnf:ambiguity)

@section{Defining parser functions}

@declare-exporting[abnf]

abnf-parser

define-abnf-parser

@section{Traversing concrete syntax values}

@declare-exporting[abnf]

@defproc[(traverse [f ((cst? . -> . any) (or/c cst? string? number?) . -> . any)]
                   [cst cst?]) any]{

When writing semantic functions, one often wishes to concentrate on
interpreting the results of matched rules, treating alternation,
concatenation, and repetition generically.

For example, if @racket[walk] is our semantic function, we may want

@(tabular #:sep @hspace[1]
          (list
           (list @racket[(walk `(/ #,(emph "i") #,(emph "cst")))]
                 "≡"
                 @racket[(walk #,(emph "cst"))])
           (list @racket[(walk `(: #,(emph "cst" (subscript "1")) #,(emph "cst" (subscript "2")) #,(elem "...")))]
                 "≡"
                 @racket[(map walk (list #,(emph "cst" (subscript "1")) #,(emph "cst" (subscript "2")) #,(elem "...")))])
           (list @racket[(walk `(* (#,(emph "cst" (subscript "1")) #,(emph "cst" (subscript "2")) #,(elem "..."))))]
                 "≡"
                 @racket[(map walk (list #,(emph "cst" (subscript "1")) #,(emph "cst" (subscript "2")) #,(elem "...")))])))

The function @racket[traverse] abstracts out this common pattern.

The function @racket[f] is called only for @tech{CST}s that are
strings, numbers, or the results of grammar rule applications. All
other kinds of @tech{CST} are handled per the equations above.

When @racket[f] is called, it is given two arguments: a function to
call to recursively process a CST, and the CST under current
consideration.

@bold{Example: Booleans.} Consider the following grammar,
@racket["bools-rules.rkt"], which accepts sequences of @tt{t}s and
@tt{f}s:

@grammar-snippet{
#lang abnf
bools = *bool
bool = true / false
true = "t"
false = "f"
}

The grammar yields raw CSTs like this:

@racketinput[(define-abnf-parser parse-bools/cst "bools-rules.rkt" bools values)
             (parse-bools/cst "ttfftff")]
@racketresultblock['(bools
                     (*
                      ((bool (/ 0 (true "t")))
                       (bool (/ 0 (true "t")))
                       (bool (/ 1 (false "f")))
                       (bool (/ 1 (false "f")))
                       (bool (/ 0 (true "t")))
                       (bool (/ 1 (false "f")))
                       (bool (/ 1 (false "f"))))))]

We could convert such a CST to a sequence of Racket booleans using a
direct recursion, handling alternation and repetition ourselves:

@racketblock[(define (cst->ast/1 cst)
               (let walk ((cst cst))
                 (match cst
                   [`(bools (* ,vs)) (map walk vs)]
                   [`(bool (/ 0 ,_)) #t]
                   [`(bool (/ 1 ,_)) #f])))]

@racketinput[(cst->ast/1 (parse-bools/cst "ttfftff"))]
@racketresultblock[(list #t #t #f #f #t #f #f)]

Alternatively, we could use @racket[traverse] to allow us to
concentrate on the portions of interest, namely the @tt{true} and
@tt{false} rules, letting @racket[traverse] take care of
@racket[map]ping, alternation, and so forth:

@racketblock[(define (cst->ast/2 cst)
               (traverse (lambda (walk cst)
                           (match cst
                             [`(true ,_) #t]
                             [`(false ,_) #f]
                             [`(,_ ,v) (walk v)])) (code:comment "recurse through all other rule tags")
                         cst))]

@racketinput[(cst->ast/2 (parse-bools/cst "ttfftff"))]
@racketresultblock[(list #t #t #f #f #t #f #f)]

The third case in the @racket[match] in @racket[cst->ast/2] is
necessary to allow @racket[traverse] to "reach through" layers of
uninteresting structure such as, in this example, the tagged CSTs from
rules @tt{bool} and @tt{bools}.

}

@section{Parser runtime support library}

@defmodule[abnf/runtime]

         (struct-out parse-input)
         (struct-out parse-result)
         (struct-out parse-error)

         analyze-parser-results
         raise-abnf-syntax-error
         raise-abnf-ambiguity-error
