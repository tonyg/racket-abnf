#lang racket/base

(provide parse-csv
         parse-csv/header
         parse-csv*
         (all-from-out "rfc4180/ast.rkt"))

(require racket/match)
(require (only-in racket/string string-replace))
(require (prefix-in : "rfc4180/ast.rkt"))
(require abnf)
(require abnf/rfc5234/core)

(define ((csv-cst->ast #:header? [header? #f] #:trim-empty? [trim-empty? #t]) cst)
  (define (make-record walk ctor v)
    (define fields (flatten (walk v)))
    (if (and trim-empty? (equal? fields '("")))
        '()
        (ctor fields)))
  (traverse (lambda (walk cst)
              (match cst
                [`(file ,v) (flatten (walk v))]
                [`(header ,v) (make-record walk (if header? :header :record) v)]
                [`(record ,v) (make-record walk :record v)]
                [`(name ,v) (walk v)]
                [`(field ,v) (walk v)]
                [`(escaped (: ,_q1 ,cs ,_q2)) (string-replace (text cs) "\"\"" "\"")]
                [`(non-escaped ,cs) (text cs)]
                [`(COMMA ,_) '()]
                [`(CRLF ,_) '()]
                ))
            cst))

(define-abnf-parser (parse-csv* #:header? header? #:trim-empty? trim-empty?)
  abnf/rfc4180/rules file
  (csv-cst->ast #:header? header? #:trim-empty? trim-empty?))

(define (parse-csv #:trim-empty? [trim-empty? #t] input)
  ((parse-csv* #:header? #f #:trim-empty? trim-empty?) input))

(define (parse-csv/header #:trim-empty? [trim-empty? #t] input)
  ((parse-csv* #:header? #t #:trim-empty? trim-empty?) input))
