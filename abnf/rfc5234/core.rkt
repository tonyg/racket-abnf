#lang racket/base

(provide text)

(require racket/match)
(require (only-in racket/list flatten))
(require "../main.rkt")

(define (text cst [f (lambda (walk cst) (walk cst))])
  (list->string
   (flatten
    (traverse (lambda (walk cst)
                (f (lambda (cst)
                     (match cst
                       [(? string? s) (string->list s)]
                       [(? number? n) (integer->char n)]
                       [`(,_tag ,v) (walk v)]
                       [other (walk other)]))
                   cst))
              cst))))
