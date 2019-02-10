#lang racket/base

(provide abnf-cst->ast)

(require racket/match)
(require (only-in racket/list flatten))

(require (prefix-in : "ast.rkt"))

(struct definition (name type body) #:prefab)

(define (abnf-cst->ast ast)
  (define (walk ast)
    (match ast
      [`(/ ,_ ,v) (walk v)]
      [`(: ,@vs) (map walk vs)]
      [`(* ,vs) (map walk vs)]

      [`(rulelist ,items) (flatten (walk items))]
      [`(rule (: ,n ,d ,e ,_)) (definition (walk n) (walk d) (walk e))]
      [`(rulename ,n) (list->string (flatten (walk n)))]
      [`(c-nl ,_) '()]
      [`(defined-as (: ,_ (/ 0 ,_) ,_)) 'define]
      [`(defined-as (: ,_ (/ 1 ,_) ,_)) 'extend]
      [`(elements (: ,a ,_)) (walk a)]
      [`(alternation ,as) (:alternation (flatten (walk as)))]
      [`(concatenation ,rs) (:concatenation (flatten (walk rs)))]
      [`(repetition (: (* ()) ,e)) (walk e)]
      [`(repetition (: (* (,r)) ,e)) (match (walk r) [(list lo hi) (:repetition lo hi (walk e))])]

      [`(ALPHA (/ ,_ ,n)) (integer->char n)]
      [`(DIGIT ,n) (integer->char n)]
      [(? string? s) (string->list s)]
      [`() '()]
      [_ (error 'abnf-cst->ast "unhandled: ~v" ast)]))
  (local-require racket/trace) (trace walk)
  (walk ast))
