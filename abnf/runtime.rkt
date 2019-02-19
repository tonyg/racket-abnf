#lang racket/base

(provide (struct-out parse-result)
         (struct-out parse-error)
         merge-error
         combine
         no-error
         succeed
         fail
         input-byte
         input-substring
         loc->srcloc
         analyze-parser-results)

;; (provide *nonterminal-stack*)
;; (define *nonterminal-stack* (box '()))

(require racket/match)

(struct parse-result (value loc) #:prefab)
(struct parse-error (message loc) #:prefab)

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

(define (input-byte bs i)
  (and (< i (bytes-length bs))
       (bytes-ref bs i)))

(define (input-substring bs i count)
  (define j (+ i count))
  (and (<= j (bytes-length bs))
       (bytes->string/latin-1 (subbytes bs i j))))

(define (loc->srcloc loc input source-name)
  (local-require (only-in racket/string string-split))
  (local-require (only-in racket/list last))
  (define lines (string-split (bytes->string/latin-1 (subbytes input 0 loc)) "\n" #:trim? #f))
  (define row (length lines))
  (define last-line (if (null? lines) "" (last lines)))
  (define col (string-length last-line))
  (srcloc source-name (+ row 1) col (+ loc 1) #f))

(define (analyze-parser-results results
                                #:incomplete-parse-error? [incomplete-parse-error? #t]
                                input-bytes source-name ks kf
                                [ka (lambda (rs)
                                      (kf "Ambiguous result"
                                          (loc->srcloc 0 input-bytes source-name)))])
  (define (handle-error e)
    (match-define (parse-error msg loc) e)
    (kf msg (loc->srcloc loc input-bytes source-name)))
  (define (handle-result r)
    (match-define (parse-result cst loc) r)
    (ks cst))
  (match results
    [(list e) (handle-error e)]
    [(list e r) ;; potentially incomplete parse
     (if (and (< (parse-result-loc r) (bytes-length input-bytes)) ;; incomplete
              incomplete-parse-error?)
         (handle-error e)
         (handle-result r))]
    [other ;; ambiguous result
     (ka other)]))
