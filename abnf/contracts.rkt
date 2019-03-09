#lang racket/base

(provide (all-defined-out))

(require racket/contract)
(require abnf/runtime)

(define parser-input/c (or/c parse-input? bytes? string?))
(define parser-output/c (cons/c parse-error? (listof parse-result?)))
(define parser-function/c (-> parser-input/c parser-output/c))
(define parser/c (->* (parser-input/c)
                      (string?
                       #:on-ambiguity (-> parser-input/c parser-output/c any/c))
                      any/c))
