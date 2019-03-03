#lang racket/base

(provide (struct-out exn:fail:abnf)
         (struct-out exn:fail:abnf:syntax)
         (struct-out exn:fail:abnf:ambiguity)
         (struct-out parse-input)
         (struct-out parse-result)
         (struct-out parse-error)
         ->parse-input
         merge-error
         combine
         no-error
         succeed
         fail
         ;; input-cache-ref
         ;; input-cache-add!
         ;; input-cache-finalize!
         input-codepoint
         input-char
         input-substring
         loc->srcloc
         analyze-parser-results
         traverse
         raise-abnf-syntax-error
         abnf-parser
         define-abnf-parser)

;; (provide *nonterminal-stack*)
;; (define *nonterminal-stack* (box '()))

(require racket/match)
(require racket/unsafe/ops)
(require (only-in racket/vector vector-copy))

(module+ test (require rackunit))

(struct exn:fail:abnf exn:fail () #:transparent)
(struct exn:fail:abnf:syntax exn:fail:abnf (input failing-ast loc) #:transparent)
(struct exn:fail:abnf:ambiguity exn:fail:abnf (input outcomes) #:transparent)

(struct parse-input (codepoints #;cache) #:prefab)
(struct parse-result (value loc) #:prefab)
(struct parse-error (failing-ast loc) #:prefab)

(define (->parse-input x)
  (cond [(parse-input? x) x]
        [(bytes? x) (parse-input (list->vector (bytes->list x)) #;(make-hasheqv))]
        [(string? x) (parse-input (list->vector (map char->integer (string->list x))))]
        [else (error '->parse-input "Expected `parse-input`, `string` or `bytes`; got ~v" x)]))

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

(define-syntax-rule (input-codepoint in i0)
  (let ((cs (parse-input-codepoints in))
        (i i0))
    (and (< i (unsafe-vector-length cs))
         (unsafe-vector-ref cs i))))

(define-syntax-rule (input-char in i)
  (let ((b (input-codepoint in i)))
    (and b (integer->char b))))

(define-syntax-rule (input-substring in i0 count)
  (let* ((cs (parse-input-codepoints in))
         (i i0)
         (j (+ i count)))
    (and (<= j (unsafe-vector-length cs))
         (list->string (map integer->char (vector->list (vector-copy cs i j)))))))

(define (loc->srcloc loc input source-name)
  (local-require (only-in racket/string string-split))
  (local-require (only-in racket/list last))
  (define lines (if (zero? loc)
                    '("")
                    (string-split (input-substring input 0 loc) "\n" #:trim? #f)))
  (define row (length lines))
  (define last-line (last lines))
  (define col (string-length last-line))
  (srcloc source-name row col (+ loc 1) #f))

(module+ test
  (let ((! (lambda (loc)
             (loc->srcloc loc (->parse-input #"abcde\r\nfghij\r\nklmno\r\n") "adhoc")))
        (p (lambda (line column position)
             (srcloc "adhoc" line column position #f))))
    (check-equal? (! 0) (p 1 0 1))
    (check-equal? (! 3) (p 1 3 4))
    (check-equal? (! 8) (p 2 1 9)) ;; NB. Racket counts *positions* differently, yielding 2 1 8 here
    (check-equal? (! 21) (p 4 0 22))))

(define (analyze-parser-results results
                                #:incomplete-parse-error? [incomplete-parse-error? #t]
                                input source-name ks kf
                                [ka (lambda (rs)
                                      (kf "Ambiguous result"
                                          (loc->srcloc 0 input source-name)))])
  (define (handle-error e)
    (match-define (parse-error failing-ast loc) e)
    (kf failing-ast (loc->srcloc loc input source-name)))
  (define (handle-result r)
    (match-define (parse-result cst loc) r)
    (ks cst))
  (match results
    [(list e) (handle-error e)]
    [(list e r) ;; potentially incomplete parse
     (if (and (< (parse-result-loc r) (vector-length (parse-input-codepoints input))) ;; incomplete
              incomplete-parse-error?)
         (handle-error e)
         (handle-result r))]
    [other ;; ambiguous result
     (ka other)]))

(define (traverse f cst)
  (define (walk cst)
    (match cst
      [`(/ ,_ ,v) (walk v)]
      [`(: ,@vs) (map walk vs)]
      [`(* ,vs) (map walk vs)]
      [`(,_tag ,_v) (f walk cst)]
      [(? string? v) (f walk v)]
      [(? number? v) (f walk v)]))
  (walk cst))

(define (format-abnf-failing-ast failing-ast)
  (local-require (prefix-in : "rfc5234/ast.rkt"))
  (match failing-ast
    [(:range b b)
     (format "Expected ~v" (make-string 1 (integer->char b)))]
    [(:range lo hi)
     (format "Expected a byte between ~a (\"~a\") and ~a (\"~a\")"
             lo
             (integer->char lo)
             hi
             (integer->char hi))]
    [(:char-val ci-str)
     (format "Expected ~v (case-insensitive)" ci-str)]))

(define (raise-abnf-syntax-error input failing-ast loc)
  (raise (exn:fail:abnf:syntax
          (format "Syntax error: ~a: ~a"
                  (srcloc->string loc)
                  (format-abnf-failing-ast failing-ast))
          (current-continuation-marks)
          input
          failing-ast
          loc)))

(define (raise-abnf-ambiguity-error input outcomes)
  (raise (exn:fail:abnf:ambiguity
          "ABNF grammar is ambiguous for given input"
          (current-continuation-marks)
          input
          outcomes)))

(define (abnf-parser #:incomplete-parse-error? [incomplete-parse-error? #t]
                     parser semantic-function)
  (lambda (input0
           [source-name "<unknown>"]
           #:on-ambiguity [handle-ambiguity raise-abnf-ambiguity-error])
    (define input (->parse-input input0))
    (analyze-parser-results (parser input)
                            #:incomplete-parse-error? incomplete-parse-error?
                            input
                            source-name
                            semantic-function
                            (lambda (failing-ast loc)
                              (raise-abnf-syntax-error input failing-ast loc))
                            (lambda (outcomes)
                              (raise-abnf-ambiguity-error input outcomes)))))

(define-syntax-rule (define-abnf-parser id cst-module rulename semantic-function)
  (define id (abnf-parser (let () (local-require cst-module) rulename) semantic-function)))
