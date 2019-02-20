#lang racket/base

(provide (struct-out parse-input)
         (struct-out parse-result)
         (struct-out parse-error)
         bytes->parse-input
         merge-error
         combine
         no-error
         succeed
         fail
         ;; input-cache-ref
         ;; input-cache-add!
         ;; input-cache-finalize!
         input-byte
         input-char
         input-substring
         loc->srcloc
         analyze-parser-results)

;; (provide *nonterminal-stack*)
;; (define *nonterminal-stack* (box '()))

(require racket/match)

(struct parse-input (bytes #;cache) #:prefab)
(struct parse-result (value loc) #:prefab)
(struct parse-error (message loc) #:prefab)

(define (bytes->parse-input bs)
  (parse-input bs #;(make-hasheqv)))

(define (merge-error e1 e2)
  (if (>= (parse-error-loc e1) (parse-error-loc e2)) e1 e2))

(define (combine err0 rss)
  (define-values (err results)
    (for/fold [(err err0) (results '())] [(rs (in-list rss))]
      (for/fold [(err err) (results results)] [(r (in-list rs))]
        (match r
          [(? parse-error? e) (values (merge-error err e) results)]
          [(? parse-result? r) (values err (cons r results))]))))
  (cons err results))

(define no-error (parse-error "no error" -1))

(define (succeed v l) (list no-error (parse-result v l)))
(define (fail m l) (list (parse-error m l)))

;; (define (input-cache-ref in key)
;;   (define c (parse-input-cache in))
;;   (if (hash-has-key? c key)
;;       #f
;;       (let ((b (box '())))
;;         (hash-set! c key b)
;;         b)))

;; (define (input-cache-add! b value)
;;   (set-box! b (cons value (unbox b))))

;; (define (input-cache-finalize! in key b)
;;   (hash-set! (parse-input-cache in) key (unbox b)))

(define (input-byte in i)
  (define bs (parse-input-bytes in))
  (and (< i (bytes-length bs))
       (bytes-ref bs i)))

(define (input-char in i)
  (define b (input-byte in i))
  (and b (integer->char b)))

(define (input-substring in i count)
  (define bs (parse-input-bytes in))
  (define j (+ i count))
  (and (<= j (bytes-length bs))
       (bytes->string/latin-1 (subbytes bs i j))))

(define (loc->srcloc loc input source-name)
  (local-require (only-in racket/string string-split))
  (local-require (only-in racket/list last))
  (define lines (string-split (bytes->string/latin-1 (subbytes (parse-input-bytes input) 0 loc))
                              "\n" #:trim? #f))
  (define row (length lines))
  (define last-line (if (null? lines) "" (last lines)))
  (define col (string-length last-line))
  (srcloc source-name (+ row 1) col (+ loc 1) #f))

(define (analyze-parser-results results
                                #:incomplete-parse-error? [incomplete-parse-error? #t]
                                input source-name ks kf
                                [ka (lambda (rs)
                                      (kf "Ambiguous result"
                                          (loc->srcloc 0 input source-name)))])
  (define (handle-error e)
    (match-define (parse-error msg loc) e)
    (kf msg (loc->srcloc loc input source-name)))
  (define (handle-result r)
    (match-define (parse-result cst loc) r)
    (ks cst))
  (match results
    [(list e) (handle-error e)]
    [(list e r) ;; potentially incomplete parse
     (if (and (< (parse-result-loc r) (bytes-length (parse-input-bytes input))) ;; incomplete
              incomplete-parse-error?)
         (handle-error e)
         (handle-result r))]
    [other ;; ambiguous result
     (ka other)]))
