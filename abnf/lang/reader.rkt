#lang racket/base

(provide (rename-out [read-abnf read-syntax]))

(require (only-in racket/port port->bytes))
(require racket/pretty)

(require "../abnf-semantics.rkt")
(require "../runtime.rkt")

;; (require "../interpret.rkt")
;; (require (prefix-in abnf: "../genboot.rkt"))
;; (define abnf-parser (rulelist->parser abnf:rulelist 'rulelist))

(require (prefix-in abnf: "../boot.rkt"))
(define abnf-parser abnf:rulelist)

(define (read-abnf src [p (current-input-port)] #:language [language #'abnf/lang])
  (define grammar (bytes->parse-input (port->bytes p)))
  (analyze-parser-results (abnf-parser grammar)
                          grammar
                          src
                          (lambda (cst)
                            #`(module abnf-module #,language
                                (_define-and-provide-abnf #,(abnf-cst->ast cst))))
                          (lambda (msg loc)
                            (raise-syntax-error 'read-abnf
                                                (format "ABNF syntax error: ~a: ~a"
                                                        msg
                                                        (srcloc->string loc))))
                          (lambda (other)
                            (error 'read-abnf
                                   "Ambiguous ABNF grammar input!\n~v\n~v"
                                   (parse-input-bytes grammar)
                                   (pretty-format other)))))
