#lang racket/base

(provide text)

(require racket/match)
(require (only-in racket/list flatten))
(require "../main.rkt")

(define (text cst)
  (list->string
   (flatten
    (let walk ((cst cst))
      (match cst
        [(? string? s) (string->list s)]
        [(? number? n) (integer->char n)]
        [other (traverse walk other)])))))
