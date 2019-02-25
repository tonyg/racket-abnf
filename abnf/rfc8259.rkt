#lang racket/base

(provide bytes->jsexpr)

(require racket/match)
(require abnf)
(require abnf/rfc5234/core)
(require (only-in json json-null))

(define (cst->ast cst)
  (traverse (lambda (walk cst)
              (match cst
                [`(JSON-text (: ,_ ,v ,_)) (walk v)]
                [`(value ,v) (walk v)]
                [`(array (: (begin-array ,_)
                            (* ())
                            (end-array ,_)))
                 '()]
                [`(array (: (begin-array ,_)
                            (* ((: ,v0 (* ((: ,_ ,vs) ...)))))
                            (end-array ,_)))
                 (map walk (cons v0 vs))]
                [`(object (: (begin-object ,_)
                             (* ())
                             (end-object ,_)))
                 (hasheq)]
                [`(object (: (begin-object ,_)
                             (* ((: ,v0 (* ((: ,_ ,vs) ...)))))
                             (end-object ,_)))
                 (make-immutable-hasheq (map walk (cons v0 vs)))]
                [`(member (: ,k ,_ ,v)) (cons (string->symbol (walk k)) (walk v))]
                [`(string (: ,_q1 ,t ,_q2)) (unescape (text t))]
                [`(number ,n) (string->number (text n))]
                [`(true ,_) #t]
                [`(false ,_) #f]
                [`(null ,_) (json-null)]))
            cst))

(define (unescape s)
  ;; TODO
  s)

(define-abnf-parser bytes->jsexpr
  "rfc8259/rules.rkt"
  JSON-text
  cst->ast)
