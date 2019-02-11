#lang racket/base

(require racket/match)
(require bitsyntax)
(require (only-in racket/promise force))
(require (only-in racket/list append-map))

(require (prefix-in : "ast.rkt"))
(require (prefix-in abnf: "boot.rkt"))

(struct parse-result (value input loc) #:prefab)
(struct parse-error (message loc) #:prefab)

(define (advance-string loc s)
  (for/fold [(loc loc)] [(c (in-string s))]
    (advance-byte loc (char->integer c))))

(define (advance-byte loc c)
  (match c
    [#x0d (struct-copy srcloc loc
            [position (+ (srcloc-position loc) 1)]
            [column 0])]
    [#x0a (struct-copy srcloc loc
            [position (+ (srcloc-position loc) 1)]
            [column 0]
            [line (+ (srcloc-line loc) 1)])]
    [#x09 (let ((col (srcloc-column loc)))
            (struct-copy srcloc loc
              [position (+ (srcloc-position loc) 1)]
              [column (+ col (- 8 (remainder col 8)))]))]
    [_ (struct-copy srcloc loc
         [position (+ (srcloc-position loc) 1)]
         [column (+ (srcloc-column loc) 1)])]))

(define (merge-error e1 e2)
  (if (>= (srcloc-position (parse-error-loc e1)) (srcloc-position (parse-error-loc e2)))
      e1
      e2))

(define (srcloc->string* loc)
  (format "~a:~a:~a" (srcloc-line loc) (srcloc-column loc) (srcloc-position loc)))

(define (make-syntax v loc) v)

(define (combine-results rs1 rs2)
  (define errs1 (filter parse-error? rs1))
  (define results1 (filter parse-result? rs1))
  (define errs2 (filter parse-error? rs2))
  (define results2 (filter parse-result? rs2))
  (append (let ((errs (append errs1 errs2)))
            (if (null? errs)
                '()
                (list (foldl merge-error (car errs) (cdr errs)))))
          results1
          results2))

(define (>>= rs k)
  (define old-errs (filter parse-error? rs))
  (define old-results (filter parse-result? rs))
  (define rs1 (append-map (match-lambda [(parse-result v i l) (k v i l)]) old-results))
  (combine-results old-errs rs1))

(define (succeed v i l)
  (list (parse-result v i l)))

(define (fail m l)
  (list (parse-error m l)))

(define (interpret ast input loc)
  (match ast
    [(:rule name item-promise)
     (>>= (interpret (force item-promise) input loc)
          (lambda (r input loc)
            (succeed (make-syntax (list name r) loc) input loc)))]
    [(:repetition min max item)
     (define left-pos loc)
     (let loop ((results-rev '()) (input input) (loc loc) (count 0))
       (match (interpret item input loc)
         [(list (? parse-error?))
          (if (< count min)
              (fail "too few repetitions" loc)
              (succeed (make-syntax (list '* (reverse results-rev)) left-pos) input loc))]
         [rs (>>= rs (lambda (r input loc)
                       (let ((count (+ count 1)))
                         (if (or (not max) (<= count max))
                             (loop (cons r results-rev) input loc count)
                             (fail "too many repetitions" loc)))))]))]
    [(:alternation items)
     (let loop ((items items) (index 0))
       (match items
         ['() '()]
         [(cons item items) (combine-results (>>= (interpret item input loc)
                                                  (lambda (r input loc)
                                                    (succeed (make-syntax (list '/ index r) loc)
                                                             input
                                                             loc)))
                                             (loop items (+ index 1)))]))]
    [(:concatenation items)
     (define left-pos loc)
     (let loop ((results-rev '()) (items items) (input input) (loc loc))
       (match items
         ['() (succeed (make-syntax (cons ': (reverse results-rev)) left-pos) input loc)]
         [(cons item items) (>>= (interpret item input loc)
                                 (lambda (r input loc)
                                   (loop (cons r results-rev) items input loc)))]))]
    [(:char-val ci-str)
     (define-values (head0 tail) (bit-string-split-at-or-false input (* 8 (string-length ci-str))))
     (define head (and head0 (bytes->string/latin-1 (bit-string->bytes head0))))
     (if (and head (string-ci=? head ci-str))
         (succeed (make-syntax head loc) tail (advance-string loc head))
         (fail (format "expected ~v" ci-str) loc))]
    [(:range lo hi)
     (bit-string-case input
       ([ head (tail :: binary) ]
        (if (<= lo head hi)
            (succeed (make-syntax head loc) tail (advance-byte loc head))
            (fail (format "input ~a out-of-range [~a-~a]" head lo hi) loc)))
       (else (fail "unexpected end of input" loc)))]))

(module+ main
  (require racket/file)
  (require racket/pretty)
  (require "abnf-semantics.rkt")
  (match (time (interpret abnf:rulelist

                          ;; #"foo = %x20 %x20\r\n"
                          ;; (srcloc "adhoc" 1 0 1 #f)

                          (file->bytes "rfc5234-section-4.abnf")
                          (srcloc "rfc5234-section-4.abnf" 1 0 1 #f)

                          ;; (file->bytes "rfc5234-appendix-b.abnf")
                          ;; (srcloc "rfc5234-appendix-b.abnf" 1 0 1 #f)

                          ;; (file->bytes "rfc5322.abnf")
                          ;; (srcloc "rfc5322.abnf" 1 0 1 #f)

                          ))
    [(list (parse-error msg loc))
     (printf "SYNTAX ERROR\n~a\n~a\n"
             msg
             (srcloc->string* loc))]
    [(list (? parse-error?) (parse-result cst remaining loc))
     (pretty-print (abnf-cst->ast cst))
     (pretty-print loc)
     (pretty-print (bit-string->bytes remaining))]
    [other
     (printf "AMBIGUOUS RESULT\n")
     (pretty-print other)])
  )
