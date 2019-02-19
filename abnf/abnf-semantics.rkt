#lang racket/base

(provide abnf-cst->ast)

(require racket/match)
(require (only-in racket/list flatten))
(require (only-in racket/port with-input-from-string port->list))

(require (prefix-in : "ast.rkt"))

(define (abnf-cst->ast ast)
  (define (walk ast)
    (match ast
      [`(/ ,_ ,v) (walk v)]
      [`(: ,@vs) (map walk vs)]
      [`(* ,vs) (map walk vs)]

      [`(rulelist ,items) (:rulelist (flatten (walk items)))]
      [`(rule (: (rulename ,n) ,d ,e ,_)) (:rule (string->symbol (text n)) (walk d) (walk e))]
      [`(rulename ,n) (:reference (string->symbol (text n)))]
      [`(defined-as (: ,_ (/ 0 ,_) ,_)) #f]
      [`(defined-as (: ,_ (/ 1 ,_) ,_)) #t]
      [`(elements (: ,a ,_)) (walk a)]
      [`(biased-choice (: ,c0 (* ()))) (walk c0)]
      [`(biased-choice (: ,c0 (* ((: ,_ "//" ,_ ,cs) ...))))
       (:biased-choice (flatten (map walk (cons c0 cs))))]
      [`(alternation (: ,c0 (* ()))) (walk c0)]
      [`(alternation (: ,c0 (* ((: ,_ "/" ,_ ,cs) ...))))
       (:alternation (flatten (map walk (cons c0 cs))))]
      [`(concatenation (: ,r (* ()))) (walk r)]
      [`(concatenation ,rs) (:concatenation (flatten (walk rs)))]
      [`(repetition (: (* ()) ,e)) (walk e)]
      [`(repetition (: (* (,r)) ,e)) (match (walk r) [(list lo hi) (:repetition lo hi (walk e))])]
      [`(repeat (/ 0 ,v)) (list (string->number (text v)) #f)]
      [`(repeat (/ 1 (: ,v "*" ,w))) (list (or (string->number (text v)) 0)
                                           (string->number (text w)))]
      [`(element ,item) (walk item)]
      [`(group (: "(" ,_ ,a ,_ ")")) (walk a)]
      [`(option (: "[" ,_ ,a ,_ "]")) (:repetition 0 1 (walk a))]
      [`(c-wsp ,_) '()]
      [`(c-nl ,_) '()]
      [`(char-val (: ,_ ,v ,_)) (:char-val (text v))]
      [`(num-val (: "%" ,v)) (walk v)]
      [`(meta (: "@" ,v ,_)) (:meta (with-input-from-string (text v) port->list))]

      [`(,(or 'bin-val 'dec-val 'hex-val) (: ,base ,digits0 (* ())))
       (:range (val base digits0) (val base digits0))]
      [`(,(or 'bin-val 'dec-val 'hex-val) (: ,base ,digits0
                                             (* ((/ 0 (* ((: "." ,digitsNs) ...)))))))
       (:concatenation (cons (val base digits0)
                             (map (lambda (digitsN) (val base digitsN)) digitsNs)))]
      [`(,(or 'bin-val 'dec-val 'hex-val) (: ,base ,digits0 (* ((/ 1 (: "-" ,digits1))))))
       (:range (val base digits0) (val base digits1))]

      [`(ALPHA (/ ,_ ,n)) (integer->char n)]
      [`(DIGIT ,n) (integer->char n)]
      [`(HEXDIG ,n) (walk n)]
      [`(VCHAR ,n) (integer->char n)]
      [`(WSP ,v) (walk v)]
      [`(SP ,n) (integer->char n)]
      [(? string? s) (string->list s)]
      [(? number? n) (integer->char n)]
      [`() '()]
      [_ (error 'abnf-cst->ast "unhandled: ~v" ast)]))

  (define (text ast)
    (list->string (flatten (walk ast))))

  (define (val base digits-ast)
    (string->number (format "#~a~a" base (text digits-ast))))

  (walk ast))
