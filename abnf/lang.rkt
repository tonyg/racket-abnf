#lang racket/base

(provide _define-and-provide-abnf)

(require (for-syntax racket/base syntax/kerncase))

(require (prefix-in : "ast.rkt"))
(require (for-syntax "compile.rkt"))

(define-syntax (_define-and-provide-abnf stx)
  (syntax-case stx ()
    [(_ rulelist-literal)
     (let ((rulelist (syntax->datum #'rulelist-literal)))
       (define compiled (compile-rulelist rulelist))
       ;; (local-require racket/pretty)
       ;; (pretty-print (syntax->datum compiled))
       compiled)]))
