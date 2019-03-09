#lang racket/base

(provide (rename-out [read-abnf read-syntax]))

(require (only-in racket/port port->string))
(require racket/pretty)

(require "../rfc5234/abnf-semantics.rkt")
(require "../runtime.rkt")

;; (require "../interpret.rkt")
;; (require (prefix-in boot: "../private/genboot-self-hosting.rkt"))
;; (define rulelist-parser (rulelist->parser boot:rulelist 'rulelist))

(require (prefix-in abnf: "../boot.rkt"))
(define rulelist-parser abnf:rulelist)

(define (read-abnf src [p (current-input-port)] #:language [language #'abnf/lang])
  (let ((parser (abnf-parser rulelist-parser
                             (lambda (cst)
                               #`(module abnf-module #,language
                                   (_define-and-provide-abnf #,(abnf-cst->ast cst)))))))
    (parser (->parse-input (port->string p)) src)))
