# racket-abnf

Racket ABNF implementation (interpreter & compiler), for directly
cutting-and-pasting from RFCs.

## Bugs

 - The error reporting is poor. Sometimes the position is off-by-one
   (?); frequently, the error message is bad. Taking strategies from
   packrat parser implementations might be a good idea?

 - Extensions "rule =/ ..." need to flatten the resulting alternations
   to a single rule-toplevel alternation, rather than a nested tree.

-----

# abnf: RFC5234-compatible ABNF \#lang

Tony Garnock-Jones
<[tonyg@leastfixedpoint.com](mailto:tonyg@leastfixedpoint.com)>

## 1. Introduction

```racket
 (require abnf) package: [abnf](https://pkgs.racket-lang.org/package/abnf)
```

This library implements a `#lang` and compiler for grammars specified in
the ABNF dialect given by [RFC
5234](http://tools.ietf.org/html/rfc5234).

Using `#lang abnf` allows grammars to be directly cut-and-pasted from
RFC documents, giving confidence that an implementation matches the
specification.

In addition, the usual advantages of parser-generator tools over
hand-implemented lexers and parsers apply: readability and
maintainability of code, and automated syntax-error handling.

The chief disadvantage of using `#lang abnf` is a modest speed penalty.
Also, input must be given as either a `string?` or a `bytes?`. Parsing
from a port is not yet supported.

As an example, compare [Racket’s own JSON
parser](https://github.com/racket/racket/blob/f720220c41551af819d21c6fffe66433abc43bbb/racket/collects/json/main.rkt#L160-L459)
to a `#lang abnf` parser directly using the ABNF from [RFC
8259](https://tools.ietf.org/html/rfc8259). The built-in parser is about
274 lines of hand-written code. It implements a JSON dialect originally
based on the [`json.org`](http://json.org/) specification. However, the
correspondence between the implementation and the `json.org` grammar is
not clear. By contrast, directly cutting-and-pasting the ABNF rules from
RFC 8259 yields [48 lines of `#lang
abnf`](https://github.com/tonyg/racket-abnf/blob/1c2d05ae97affc90e1d5cd3924f867ad5f7ac81d/abnf/rfc8259/rules.rkt)
for the grammar itself, which combine with about [50 lines of
hand-written
code](https://github.com/tonyg/racket-abnf/blob/1c2d05ae97affc90e1d5cd3924f867ad5f7ac81d/abnf/rfc8259.rkt#L10-L62)
to produce a compatible JSON reader that conforms to the RFC’s own
grammar. At the time of writing, the `#lang abnf` parser is
approximately 5–10× slower than the hand-written parser.

## 2. Theory of operation

The idea of separating specification of concrete syntax from abstract
syntax and from semantics in general was taken from the
[Ohm](https://ohmlang.github.io/) parser generator. See also the paper
[“Modular Semantic Actions”, Warth, Dubroy and Garnock-Jones, DLS
2016](https://ohmlang.github.io/pubs/dls2016/modular-semantic-actions.pdf).This
library, unlike many other parser generators, separates parsing of an
input into two separate stages.

First, a grammar given in a `#lang abnf` source file translates the
input to a _concrete syntax tree_ \(_CST_\).See the documentation for
the `cst?` predicate for a description of the structure of CST values
produced by this library. Second, the CST is interpreted by a _semantic
function_ to produce an effect, a final value, or an _abstract syntax
tree_ \(_AST_\).

A CST preserves almost every detail of the surface syntax of the input
as analyzed by the rules of the grammar, including whitespace and a
complete record of which grammatical rules were applied where. By
contrast, an AST abstracts away much of the syntactic detail of an
input, representing the essential structure of the input in terms of the
language being implemented.

Separation of concrete from abstract syntax allows for reuse of grammars
in many different contexts. Indeed, one of the main motivations for this
library was to be able to directly cut and paste ABNF specifications
from RFCs, keeping a close correspondence between a grammar and its
defining document, without being required to alter it syntactically or
to insert semantic actions.

## 3. Example: Arithmetic

To get an overview of the library and `#lang`, let’s look at a simple
language of arithmetic. First, we will specify the language’s concrete
syntax. Next, we will move on to direct interpretation from the concrete
syntax. Finally, we will produce an AST from a CST, and switch to
interpretation of the AST.

### 3.1. Grammar and concrete syntax

Consider the following Racket source file, `arithmetic-rules.rkt`:

`#lang abnf`                                           
`expr`   `= term "+" expr / term "-" expr / term`      
`term`   `= factor "*" term / factor "/" term / factor`
`factor = "(" expr ")" / num`                          
`num`    `= *SP *DIGIT *SP`                            
`SP`     `= %x20 / %x09 / %x0d / %x0a`                 
`DIGIT`  `= %x30-39`                                   

A program can produce a CST for a given input as follows:

```racket
> (require abnf)                                                              
  (define-abnf-parser parse-arithmetic/cst "arithmetic-rules.rkt" expr values)
  (parse-arithmetic/cst "10 + 20 * 30 - 40")                                  
```

Every parser resulting from `define-abnf-parser` always produces a CST
conforming to `cst?` as its output. The CST produced for the particular
parser and input given is the fearsome-looking S-expression:

```racket
  '(expr (/ 0 (: (term                                                             
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
                                                    (* ())))))))))))))))           
```

The utility `text`, imported via `(require abnf/rfc5234/core)`, recovers
the input text from a given CST:

```racket
> (require abnf/rfc5234/core)                      
  (text (parse-arithmetic/cst "10 + 20 * 30 - 40"))
```

`  "10 + 20 * 30 - 40"`

### 3.2. Direct interpretation

We know from the documentation of the `cst?` predicate and the
definition of our grammar above that we can write a direct interpreter
for arithmetic CSTs as follows:

```racket
(define (eval-cst cst)                                         
  (match cst                                                   
    [`(expr (/ 0 (: ,x "+" ,y))) (+ (eval-cst x) (eval-cst y))]
    [`(expr (/ 1 (: ,x "-" ,y))) (- (eval-cst x) (eval-cst y))]
    [`(expr (/ 2 ,z)) (eval-cst z)]                            
                                                               
    [`(term (/ 0 (: ,x "*" ,y))) (* (eval-cst x) (eval-cst y))]
    [`(term (/ 1 (: ,x "/" ,y))) (/ (eval-cst x) (eval-cst y))]
    [`(term (/ 2 ,z)) (eval-cst z)]                            
                                                               
    [`(factor (/ 0 (: "(" ,z ")"))) (eval-cst z)]              
    [`(factor (/ 1 ,z)) (eval-cst z)]                          
    [`(num (: ,_ ,digits ,_)) (string->number (text digits))]))
```

The example above evaluates as expected:

`> (eval-cst` `(parse-arithmetic/cst` `"10 + 20 * 30 - 40"))`

`  570`

### 3.3. Building and interpreting an AST

Many languages are too complex to interpret directly from concrete
syntax. A useful intermediate step is to define an abstract syntax and
to translate input concrete syntax to abstract syntax before processing
it further.

For our simple arithmetic language, a single syntax tree node type
suffices:

`> (struct` `binop` `(function` `arg0` `arg1)` `#:prefab)`

Our semantic function is almost identical to the evaluator above:

```racket
(define (cst->ast cst)                                               
  (match cst                                                         
    [`(expr (/ 0 (: ,x "+" ,y))) (binop + (cst->ast x) (cst->ast y))]
    [`(expr (/ 1 (: ,x "-" ,y))) (binop - (cst->ast x) (cst->ast y))]
    [`(expr (/ 2 ,z)) (cst->ast z)]                                  
                                                                     
    [`(term (/ 0 (: ,x "*" ,y))) (binop * (cst->ast x) (cst->ast y))]
    [`(term (/ 1 (: ,x "/" ,y))) (binop / (cst->ast x) (cst->ast y))]
    [`(term (/ 2 ,z)) (cst->ast z)]                                  
                                                                     
    [`(factor (/ 0 (: "(" ,z ")"))) (cst->ast z)]                    
    [`(factor (/ 1 ,z)) (cst->ast z)]                                
    [`(num (: ,_ ,digits ,_)) (string->number (text digits))]))      
```

It produces abstract syntax trees that capture the essence of the input
expression without details of its syntactic presentation such as
whitespace or grouping parentheses:

`> (cst->ast` `(parse-arithmetic/cst` `"10 + 20 * 30 - 40"))`

`  (binop + 10 (binop - (binop * 20 30) 40))`

We can then write an interpreter for our abstract syntax:

```racket
(define (eval-ast ast)                           
  (match ast                                     
    [(binop f x y) (f (eval-ast x) (eval-ast y))]
    [(? number? n) n]))                          
```

The interpreter produces the same results as the direct CST interpreter:

`> (eval-ast` `(cst->ast` `(parse-arithmetic/cst` `"10 + 20 * 30 -
40")))`

`  570`

The combination of a semantic function with a concrete grammar is so
common that it is included in the syntax of `define-abnf-parser`. We can
define an AST-producing variant parser by supplying `cst->ast` instead
of `values` to `define-abnf-parser`:

`(define-abnf-parser` `parse-arithmetic` `"arithmetic-rules.rkt"` `expr` `cst->ast)`

Then `parse-arithmetic` produces AST values directly:

`> (parse-arithmetic` `"10 + 20 * 30 - 40")`

`  (binop + 10 (binop - (binop * 20 30) 40))`

## 4. `#lang abnf` source files

abnf per rfc, plus // and \# and `@require` and `@biased-choice` and
`@unbiased-choice` and `@greedy-repetition` and `@non-greedy-repetition`

how imports and exports work

writing compatible parsers by hand \(cross-reference to parser inputs
and outputs\)

## 5. Parser functions, their inputs, and their outputs

```racket
parser-input/c : contract? = (or/c parse-input? bytes? string?)   
parser-output/c : contract?                                       
 = (cons/c parse-error? (listof parse-result?))                   
parser-function/c : contract?                                     
 = (parser-input/c . -> . parser-output/c)                        
parser/c : contract?                                              
 = (->* (parser-input/c)                                          
        (string?                                                  
         #:on-ambiguity (-> parser-input/c parser-output/c any/c))
        any/c)                                                    
```

An _ABNF parser function_ is any function that accepts a
`parser-input/c` input, namely anything accepted by `->parse-input`, and
produces an output conforming to `parser-output/c`.

Because working directly with the output of a parser function is
awkward, parsers defined via `abnf-parser` or `define-abnf-parser` make
use of `analyze-parser-results` to give a simpler interface conforming
to `parser/c`: such parsers accept an input as usual, but return results
directly for successful parses, generally raising exceptions for parse
errors or parse ambiguities.

```racket
(->parse-input i) -> parse-input?
  i : parser-input/c             
```

Converts its argument into a form suitable for use with a parser
function. A `bytes?` input is identity-mapped, byte-for-byte, to a
sequence of codepoints. A `string?` input is taken as a sequence of
Unicode codepoints. A `parse-input?` argument is returned unmodified.

```racket
(abnf-parser                                                     
             [#:incomplete-parse-error? incomplete-parse-error?] 
              parser                                             
              semantic-function)                                 
 -> parser/c                                                     
  incomplete-parse-error? : boolean? = #t                        
  parser : parser-function/c                                     
  semantic-function : (cst? . -> . any/c)                        
```

Produces a `parser/c` that, when given input, passes the input to
`parser` and analyzes the resulting `parser-output/c` via
`analyze-parser-results`, making use of the given `semantic-function`.
The produced `parser/c` returns the result of `analyze-parser-results`.

```racket
(define-abnf-parser id cst-module rulename semantic-function)
```

Expands to

```racket
(define id (abnf-parser (let ()                     
                          (local-require cst-module)
                          rulename)                 
                        semantic-function))         
```

```racket
(cst? maybe-cst) -> boolean?
  maybe-cst : any/c         
```

The `cst?` predicate recognises valid CSTs as produced by parsers
generated by this library.

The CST resulting from a parse is an S-expression whose shape is
constrained by the grammar driving the parse.

The base-case, simplest CSTs are those resulting from literal matches.
These are either a string, corresponding to a matched ABNF literal
string \(e.g. `"literal string"`\), or an integer codepoint number,
corresponding to a matched ABNF range \(e.g. `%x41-5A`\).

All other CSTs are built recursively:

* alternatives in the grammar produce (/ `_i_` `_cst_`)`, where _i_ is
  the index of the matched alternative;

* sequences produce (: `_cst1_` `_cst2_` `...`)`;

* repetitions produce (* (`_cst1_` `_cst2_` `...`))`; and finally,

* each matched rule from the grammar produces (`_rulename_` `_cst_`)`.

```racket
(struct exn:fail:abnf exn:fail ()                                 
    #:transparent)                                                
(struct exn:fail:abnf:syntax exn:fail:abnf (input failing-ast loc)
    #:transparent)                                                
  input : parse-input?                                            
  failing-ast : cst?                                              
  loc : srcloc?                                                   
(struct exn:fail:abnf:ambiguity exn:fail:abnf (input outcomes)    
    #:transparent)                                                
  input : parse-input?                                            
  outcomes : parser-output/c                                      
```

These exceptions are signalled by parsers resulting from `abnf-parser`
or `define-abnf-parser`.

An instance of `exn:fail:abnf:syntax` is raised when a parser cannot
produce a complete and valid parse of a given input.

An instance of `exn:fail:abnf:ambiguity` is raised when a parser
produces more than one complete and valid parse of a given input.

The structure type `exn:fail:abnf` serves merely as a convenient
supertype for blanket parse-exception handling.

## 6. Traversing concrete syntax values

```racket
(traverse f cst) -> any/c                                           
  f : ((cst? . -> . any/c) (or/c cst? string? number?) . -> . any/c)
  cst : cst?                                                        
```

When writing semantic functions, one often wishes to concentrate on
interpreting the results of matched rules, treating alternation,
concatenation, and repetition generically.

For example, if `walk` is our semantic function, we may want

`(walk `(/ `_i_` `_cst_`))`             ≡ `(walk `_cst_`)`                           
`(walk `(: `_cst1_` `_cst2_` `...`))`   ≡ `(map walk (list `_cst1_` `_cst2_` `...`))`
`(walk `(* (`_cst1_` `_cst2_` `...`)))` ≡ `(map walk (list `_cst1_` `_cst2_` `...`))`

The function `traverse` abstracts out this common pattern.

The function `f` is called only for CSTs that are strings, numbers, or
the results of grammar rule applications. All other kinds of CST are
handled per the equations above.

When `f` is called, it is given two arguments: a function to call to
recursively process a CST, and the CST under current consideration.

**Example: Booleans.** Consider the following grammar,
`"bools-rules.rkt"`, which accepts sequences of `t`s and `f`s:

`#lang abnf`         
`bools = *bool`      
`bool = true / false`
`true = "t"`         
`false = "f"`        

The grammar yields raw CSTs like this:

```racket
> (define-abnf-parser parse-bools/cst "bools-rules.rkt" bools values)
  (parse-bools/cst "ttfftff")                                        
```

```racket
  '(bools                        
    (*                           
     ((bool (/ 0 (true "t")))    
      (bool (/ 0 (true "t")))    
      (bool (/ 1 (false "f")))   
      (bool (/ 1 (false "f")))   
      (bool (/ 0 (true "t")))    
      (bool (/ 1 (false "f")))   
      (bool (/ 1 (false "f"))))))
```

We could convert such a CST to a sequence of Racket booleans using a
direct recursion, handling alternation and repetition ourselves:

```racket
(define (cst->ast/1 cst)              
  (let walk ((cst cst))               
    (match cst                        
      [`(bools (* ,vs)) (map walk vs)]
      [`(bool (/ 0 ,_)) #t]           
      [`(bool (/ 1 ,_)) #f])))        
```

`> (cst->ast/1` `(parse-bools/cst` `"ttfftff"))`

`  (list #t #t #f #f #t #f #f)`

Alternatively, we could use `traverse` to allow us to concentrate on the
portions of interest, namely the `true` and `false` rules, letting
`traverse` take care of `map`ping, alternation, and so forth:

```racket
(define (cst->ast/2 cst)                                              
  (traverse (lambda (walk cst)                                        
              (match cst                                              
                [`(true ,_) #t]                                       
                [`(false ,_) #f]                                      
                [`(,_ ,v) (walk v)])) ; recurse through all other rule
tags                                                                  
            cst))                                                     
```

`> (cst->ast/2` `(parse-bools/cst` `"ttfftff"))`

`  (list #t #t #f #f #t #f #f)`

The third case in the `match` in `cst->ast/2` is necessary to allow
`traverse` to "reach through" layers of uninteresting structure such as,
in this example, the tagged CSTs from rules `bool` and `bools`.

## 7. Parser runtime support library

```racket
 (require abnf/runtime) package: [abnf](https://pkgs.racket-lang.org/package/abnf)
```

```racket
(struct parse-input (codepoints))                   
  codepoints : (vectorof exact-nonnegative-integer?)
```

Internal structure for use in parsing. Contains a `vector` of codepoint
integers, which are interpreted as unicode code points when compared
against ABNF literal strings, and as plain integers when compared
against ABNF ranges.

```racket
(struct parse-result (value loc)) 
  value : cst?                    
  loc : exact-nonnegative-integer?
```

Represents a successful partial parse of a portion of the input. The
`value` is the resulting CST, and the `loc` is the position in the
parsed input immediately following this successful parse.

```racket
(struct parse-error (failing-ast loc))
  failing-ast : any/c                 
  loc : exact-nonnegative-integer?    
```

Represents a parse error. The `failing-ast` is an ABNF-specific
indicator of which portion of the grammar failed to match, and the `loc`
is the position in the input where the error occurred.

```racket
(analyze-parser-results                                                     
                         results                                            
                        [#:incomplete-parse-error? incomplete-parse-error?] 
                         input                                              
                         source-name                                        
                         ks                                                 
                         kf                                                 
                        [ka])                                               
 -> any/c                                                                   
  results : parser-output/c                                                 
  incomplete-parse-error? : boolean? = #t                                   
  input : parse-input?                                                      
  source-name : string?                                                     
  ks : (cst? . -> . any/c)                                                  
  kf : (any/c srcloc? . -> . any/c)                                         
  ka : (parser-output/c . -> . any/c)                                       
     = (lambda (rs)                                                         
         (kf "Ambiguous result"                                             
             (loc->srcloc 0 input source-name)))                            
```

TODO

```racket
(raise-abnf-syntax-error input                      
                         failing-ast                
                         loc)        -> any/c       
  input : parse-input?                              
  failing-ast : any/c                               
  loc : srcloc?                                     
(raise-abnf-ambiguity-error input outcomes) -> any/c
  input : parse-input?                              
  outcomes : parser-output/c                        
```

Utilities used primarily with `analyze-parser-results` to signal
`exn:fail:abnf:syntax` and `exn:fail:abnf:ambiguity` errors,
respectively.

```racket
(convert-all-results f)                   
 -> (input outcomes . -> . (listof any/c))
  f : (cst? . -> . any/c)                 
```

Applies `f` to each `parse-result?` in the `outcomes`, but passes
through each `parse-error?` unmodified.
