#lang racket/base

(provide parse-message
         parse-message-header
         (all-from-out "rfc5322/ast.rkt")

         fix-crlfs)

(require racket/match)
(require (only-in racket/string string-trim string-split string-join))
(require (only-in racket/list append-map))
(require abnf)
(require (prefix-in core: abnf/rfc5234/core))
(require (prefix-in : "rfc5322/ast.rkt"))

(module+ test (require rackunit))

(define (text cst)
  (string-trim (core:text cst)))

(define (trim-trailing-: s) (string-trim #:left? #f #:right? #t s ":"))

(define (H ctor k v . fields) (apply ctor (trim-trailing-: k) (text v) fields))

(define (unescape-quoted-pairs s)
  (regexp-replace* #px"\\\\(.)" s (lambda (_ c) c)))

(module+ test
  (check-equal? (unescape-quoted-pairs "this\\[is\\]that") "this[is]that")
  (check-equal? (unescape-quoted-pairs "") "")
  (check-equal? (unescape-quoted-pairs "\\") "\\"))

(define (cst->ast cst)
  (define f (lambda (walk cst)
              (match cst
                [`(message (: ,fs (* ()))) (:message (walk fs) "")]
                [`(message (: ,fs (* ((: ,_crlf ,b))))) (:message (walk fs) (walk b))]
                [`(fields (* (,@ms))) (map walk ms)]
                [`(body ,b) (core:text b)]

                [`(mailbox-list (/ 0 (: ,m0 (* ((: "," ,ms) ...))))) (map walk (cons m0 ms))]
                [`(mailbox ,n) (walk n)]
                [`(address-list (/ 0 (: ,a0 (* ((: "," ,as) ...))))) (map walk (cons a0 as))]
                [`(address ,x) (walk x)]
                [`(group (: ,dn ":" ,gl ";" ,_)) (:group (walk dn) (flatten (walk gl)))]
                [`(group-list (/ 0 ,ms)) (walk ms)]
                [`(group-list (/ 1 ,_)) '()]
                [`(group-list (/ 2 ,_)) '()]
                [`(name-addr (: (* (,n ...)) ,a))
                 (struct-copy :address (walk a) [display-name (and (pair? n) (walk (car n)))])]
                [`(addr-spec ,a) (:address #f (text a))]
                [`(path (/ 0 ,a)) (walk a)]
                [`(path (/ 1 ,_)) ""]
                [`(display-name ,p) (text p)]
                [`(angle-addr (/ 0 (: ,_ "<" ,a ">" ,_))) (walk a)]
                [`(angle-addr (/ 1 ,o)) (walk o)]
                [`(obs-angle-addr (: ,_ "<" ,r ,a ">" ,_)) (:routed-addr (walk r) (walk a))]
                [`(obs-route (: ,ds ":")) (walk ds)]
                [`(obs-domain-list (: ,_ "@" ,d0 (* (,@pieces))))
                 (map walk (cons d0 (append-map (match-lambda
                                                  [`(: "," ,_ (* ())) '()]
                                                  [`(: "," ,_ (* (("@" ,d)))) (list d)])
                                                pieces)))]
                [`(domain ,d) (walk d)]
                [`(dot-atom ,_ ,t ,_) (core:text t)]
                [`(domain-literal (: ,_ "[" ,t ,_ "]" ,_)) (unescape-quoted-pairs (text t))]
                [`(obs-domain ,t) (text t)]
                [`(msg-id (: ,_ "<" ,l "@" ,r ">" ,_)) (format "~a@~a" (core:text l) (core:text r))]

                [`(date-time (: (* ((: ,dow ",") ...)) ,d ,t ,_))
                 (:date-time (and (pair? dow) (walk (car dow))) (walk d) (walk t))]
                [`(day-of-week (/ 0 (: ,_ ,n))) (walk n)]
                [`(day-of-week (/ 1 (obs-day-of-week (: ,_ ,n ,_)))) (walk n)]
                [`(day-name (/ ,n ,_)) (modulo (+ n 1) 7)] ;; RFC5322 has 0=Mon, Racket has 0=Sun
                [`(date (: ,d ,m ,y)) (:date (walk y) (walk m) (string->number (core:text d)))]
                [`(month (/ ,n ,_)) (+ n 1)]
                [`(year (/ 0 ,y)) (string->number (text y))]
                [`(year (/ 1 ,y)) (+ 1900 (string->number (text y)))]
                [`(time (: (time-of-day (: ,h ":" ,m (* ((: ":" ,s) ...)))) ,z))
                 (:time (walk h) (walk m) (if (pair? s) (walk (car s)) 0) (walk z))]
                [`(hour ,h) (string->number (core:text h))]
                [`(minute ,m) (string->number (core:text m))]
                [`(second ,s) (string->number (core:text s))]
                [`(zone ,z) (text z)] ;; TODO: analyze further

                [`(received-token (/ ,_ ,x)) (walk x)]
                [`(word ,x) (walk x)]
                [`(atom ,t) (text t)]
                [`(quoted-string (: ,_ ,q1 ,t ,_ ,q2 ,_)) (unescape-quoted-pairs (core:text t))]
                [`(phrase (/ 0 (* (,@ws)))) (map walk ws)]
                [`(phrase (/ 1 ,p)) (walk p)]
                [`(obs-phrase (: ,w0 (* (,wN ...))))
                 (cons (walk w0)
                       (append-map (match-lambda [`(/ 0 ,w) (list (walk w))]
                                                 [`(/ 1 ".") (list ".")]
                                                 [`(/ 2 ,_) '()])
                                   wN))]

                [`(return (: ,k ,v ,_)) (H :return k v (walk v))]
                [`(received (: ,k (* (,t ...)) ";" ,dt ,_))
                 (H :received k `(: (* (,@t)) ";" ,dt) (map walk t) (walk dt))]
                [`(orig-date (: ,k ,v ,_)) (H :orig-date k v (walk v))]
                [`(from (: ,k ,v ,_)) (H :from k v (walk v))]
                [`(sender (: ,k ,v ,_)) (H :sender k v (walk v))]
                [`(to (: ,k ,v ,_)) (H :to k v (walk v))]
                [`(cc (: ,k ,v ,_)) (H :cc k v (walk v))]
                [`(bcc (: ,k ,v ,_)) (H :bcc k v (walk v))]
                [`(reply-to (: ,k ,v ,_)) (H :reply-to k v (walk v))]
                [`(message-id (: ,k ,v ,_)) (H :message-id k v (walk v))]
                [`(in-reply-to (: ,k ,v ,_)) (H :in-reply-to k v (walk v))]
                [`(references (: ,k ,v ,_)) (H :references k v (walk v))]
                [`(subject (: ,k ,v ,_)) (H :subject k v)]
                [`(comments (: ,k ,v ,_)) (H :comments k v)]
                [`(resent-date (: ,k ,v ,_)) (H :resent-date k v (walk v))]
                [`(resent-from (: ,k ,v ,_)) (H :resent-from k v (walk v))]
                [`(resent-sender (: ,k ,v ,_)) (H :resent-sender k v (walk v))]
                [`(resent-to (: ,k ,v ,_)) (H :resent-to k v (walk v))]
                [`(resent-cc (: ,k ,v ,_)) (H :resent-cc k v (walk v))]
                [`(resent-bcc (: ,k ,v ,_))
                 (H :resent-bcc k v (match v
                                      [`(* ()) '()]
                                      [`(* ((/ 0 ,aa))) (walk aa)]
                                      [`(* ((/ 1 ,_))) '()]))]
                [`(resent-msg-id (: ,k ,v ,_)) (H :resent-msg-id k v (walk v))]
                [`(optional-field (: ,k ":" ,v ,_)) (:generic-header (text k) (text v))]

                ;; Extensions
                [`(message-header (: (/ ,_ ,f) ,_)) (walk f)]

                )))
  ;; (local-require racket/trace) (trace f)
  (traverse f cst))

(define-abnf-parser parse-message
  "rfc5322/rules.rkt"
  message
  cst->ast)

(define parse-message-header
  (abnf-parser (let ()
                 (local-require "rfc5322/extensions.rkt")
                 message-header)
               cst->ast
               #:incomplete-parse-error? #f))

(define (string-split* s sep)
  (if (equal? s "") ;; undo the "special case" in string-split :-/
      '("")
      (string-split s sep #:trim? #f)))

(define (fix-crlfs s)
  ;; RFC 2822 §2.3 CR and LF MUST not appear outside a CRLF combination, but hey
  (string-join (append-map (lambda (s) (string-split* s "\n"))
                           (string-split* s "\r\n"))
               "\r\n"))

(module+ test
  (check-equal? (fix-crlfs "a\r\n\r\nb") "a\r\n\r\nb")
  (check-equal? (fix-crlfs "x\ny\na\r\n\r\nb") "x\r\ny\r\na\r\n\r\nb"))
