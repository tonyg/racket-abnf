#lang racket/base

(provide parse-message
         (all-from-out "rfc5322/ast.rkt"))

(require racket/match)
(require (only-in racket/string string-trim))
(require abnf)
(require (prefix-in core: abnf/rfc5234/core))
(require (prefix-in : "rfc5322/ast.rkt"))

(define (text cst)
  (string-trim (core:text cst)))

(define (cst->ast cst)
  (define f (lambda (walk cst)
              (match cst
                [`(message (: ,fs (* ()))) (:message (walk fs) "")]
                [`(message (: ,fs (* ((: ,_crlf ,b))))) (:message (walk fs) (walk b))]
                [`(fields (* (,@ms))) (map walk ms)]
                [`(body ,b) (core:text b)]

                [`(mailbox-list (/ 0 (: ,m0 (* (("," ,ms) ...))))) (map walk (cons m0 ms))]
                [`(mailbox ,n) (walk n)]
                [`(addr-spec ,a) (:address #f (text a))]

                [`(from (: ,_ ,ms ,_)) (:from "From" (text ms) (walk ms))]
                [`(subject (: ,_ ,s ,_)) (:subject "Subject" (text s))]
                )))
  ;; (local-require racket/trace) (trace f)
  (traverse f cst))

(define-abnf-parser parse-message
  "rfc5322/rules.rkt"
  message
  cst->ast)
