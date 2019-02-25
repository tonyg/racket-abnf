#lang racket/base

(provide text)

(require racket/match)
(require (only-in racket/list flatten))
(require "../main.rkt")

(define (text cst)
  (list->string
   (flatten
    (traverse (lambda (walk cst)
                (match cst
                  [(? string? s) (string->list s)]
                  [(? number? n) (integer->char n)]
                  [`(,_tag ,v) (walk v)]
                  [other (walk other)]))
              cst))))
