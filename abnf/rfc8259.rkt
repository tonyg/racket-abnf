#lang racket/base

(provide bytes->jsexpr)

(require racket/match)
(require abnf)
(require abnf/rfc5234/core)
(require (only-in json json-null))

(module+ test (require rackunit))

(define (cst->ast cst)
  (traverse
   (lambda (walk cst)
     (match cst
       [`(JSON-text (: ,_ ,v ,_)) (walk v)]
       [`(value ,v) (walk v)]
       [`(array (: (begin-array ,_) (* ()) (end-array ,_))) '()]
       [`(array (: (begin-array ,_) (* ((: ,v0 (* ((: ,_ ,vs) ...))))) (end-array ,_)))
        (map walk (cons v0 vs))]
       [`(object (: (begin-object ,_) (* ()) (end-object ,_))) (hasheq)]
       [`(object (: (begin-object ,_) (* ((: ,v0 (* ((: ,_ ,vs) ...))))) (end-object ,_)))
        (make-immutable-hasheq (map walk (cons v0 vs)))]
       [`(member (: ,k ,_ ,v)) (cons (string->symbol (walk k)) (walk v))]
       [`(string (: ,_q1 (* (,@cs)) ,_q2)) (unescape-json-string cs)]
       [`(number ,n) (string->number (text n))]
       [`(true ,_) #t]
       [`(false ,_) #f]
       [`(null ,_) (json-null)]))
   cst))

(define (leading-codepoint? c) (<= #xd800 c #xdbff))
(define (trailing-codepoint? c) (<= #xdc00 c #xdfff))
(define (surrogate-error m c) (error 'unescape-json-string "~a: ~a" m c))

(define (unescape-json-string cs) ;; TODO: Better error reporting. Needs CST locations??
  (list->string
   (let loop ((cs cs))
     (match cs
       [`() '()]
       [`((char (/ 0 (unescaped (/ ,_ ,n)))) ,@cs) (cons (integer->char n) (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 0 ,_)))) ,@cs) (cons #\" (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 1 ,_)))) ,@cs) (cons #\\ (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 2 ,_)))) ,@cs) (cons #\/ (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 3 ,_)))) ,@cs) (cons #\backspace (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 4 ,_)))) ,@cs) (cons #\page (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 5 ,_)))) ,@cs) (cons #\newline (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 6 ,_)))) ,@cs) (cons #\return (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 7 ,_)))) ,@cs) (cons #\tab (loop cs))]
       [`((char (/ 1 (: ,_backslash (/ 8 (: ,_u ,digits))))) ,@cs)
        (define n0 (string->number (text digits) 16))
        (cond [(leading-codepoint? n0)
               (match cs
                 [`((char (/ 1 (: ,_backslash (/ 8 (: ,_u ,digits))))) ,@cs)
                  (define n1 (string->number (text digits) 16))
                  (cond
                    [(trailing-codepoint? n1)
                     (cons (integer->char (+ (* 1024 (- n0 #xd800)) (- n1 #xdc00) #x10000))
                           (loop cs))]
                    [(leading-codepoint? n1) (surrogate-error "Unexpected leading codepoint" n1)]
                    [else (surrogate-error "Unpaired surrogate" n0)])]
                 [_ (surrogate-error "Unpaired surrogate" n0)])]
              [(trailing-codepoint? n0) (surrogate-error "Unexpected trailing codepoint" n0)]
              [else (cons (integer->char n0) (loop cs))])]))))

(define-abnf-parser bytes->jsexpr
  "rfc8259/rules.rkt"
  JSON-text
  cst->ast)

(module+ test
  (check-equal? (bytes->jsexpr #"{}") (hasheq))
  (define z-water-bass-bytes #"\x7A\xE6\xB0\xB4\xF0\x9D\x84\x9E")
  (define z-water-bass-string (list->string (map integer->char '(122 27700 119070))))

  (define j-zwb-1 "\"\\u007a\\u6c34\\ud834\\udd1e\"")
  (define j-zwb-2 "\"z\\u6C34\\uD834\\uDD1E\"")
  (define j-zwb-3 "\"z\u6c34\ud834\udd1e\"")
  (define j-zwb-4 (string-append "\"" z-water-bass-string "\""))

  (check-equal? (bytes->jsexpr j-zwb-1) z-water-bass-string)
  (check-equal? (bytes->jsexpr j-zwb-2) z-water-bass-string)
  (check-equal? (bytes->jsexpr j-zwb-3) z-water-bass-string)
  (check-equal? (bytes->jsexpr j-zwb-4) z-water-bass-string)

  (check-equal? (bytes->jsexpr #"\"\\b\\n\\r\\f\\t\\\\\\\"\\/\"") "\b\n\r\f\t\\\"/"))
