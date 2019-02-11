#lang racket/base

(require racket/match)
(require (only-in racket/promise force))
(require (only-in racket/list append-map))

(require (prefix-in : "ast.rkt"))
(require (prefix-in abnf: "boot.rkt"))

(struct parse-result (value loc) #:prefab)
(struct parse-error (message loc) #:prefab)

(define (advance-string loc s)
  (for/fold [(loc loc)] [(c (in-string s))]
    (advance-byte loc (char->integer c))))

(define (advance-byte loc c)
  (let ((loc (struct-copy srcloc loc [position (+ (srcloc-position loc) 1)])))
    (match c
      [#x0d (struct-copy srcloc loc [column 0])]
      [#x0a (struct-copy srcloc loc [column 0] [line (+ (srcloc-line loc) 1)])]
      [#x09 (let ((col (srcloc-column loc)))
              (struct-copy srcloc loc [column (+ col (- 8 (remainder col 8)))]))]
      [_ (struct-copy srcloc loc [column (+ (srcloc-column loc) 1)])])))

(define (merge-error e1 e2)
  (if (>= (srcloc-position (parse-error-loc e1)) (srcloc-position (parse-error-loc e2)))
      e1
      e2))

(define (srcloc->string* loc)
  (format "~a:~a:~a" (srcloc-line loc) (srcloc-column loc) (srcloc-position loc)))

(define (make-syntax v loc) v)

(define (combine-results rs1 rs2)
  (append (let ((errs (append (filter parse-error? rs1) (filter parse-error? rs2))))
            (if (null? errs)
                '()
                (list (foldl merge-error (car errs) (cdr errs)))))
          (filter parse-result? rs1)
          (filter parse-result? rs2)))

(define (>>= rs k)
  (define rs1 (append-map (match-lambda [(parse-result v l) (k v l)]) (filter parse-result? rs)))
  (combine-results (filter parse-error? rs) rs1))

(define (succeed v l) (list (parse-result v l)))
(define (fail m l) (list (parse-error m l)))

(define (loc->index loc) (- (srcloc-position loc) 1))

(define (interpret ast input loc)
  (define input-length (bytes-length input))
  (define (walk ast loc)
    (match ast
      [(:rule name item-promise)
       (>>= (walk (force item-promise) loc)
            (lambda (r loc) (succeed (make-syntax (list name r) loc) loc)))]
      [(:repetition min max item)
       (define left-pos loc)
       (let loop ((results-rev '()) (loc loc) (count 0))
         (match (walk item loc)
           [(list (? parse-error?))
            (if (< count min)
                (fail "too few repetitions" loc)
                (succeed (make-syntax (list '* (reverse results-rev)) left-pos) loc))]
           [rs (>>= rs (lambda (r loc)
                         (let ((count (+ count 1)))
                           (if (or (not max) (<= count max))
                               (loop (cons r results-rev) loc count)
                               (fail "too many repetitions" loc)))))]))]
      [(:alternation items)
       (let loop ((items items) (index 0))
         (match items
           ['() '()]
           [(cons item items) (combine-results (>>= (walk item loc)
                                                    (lambda (r loc)
                                                      (succeed (make-syntax (list '/ index r) loc)
                                                               loc)))
                                               (loop items (+ index 1)))]))]
      [(:concatenation items)
       (define left-pos loc)
       (let loop ((results-rev '()) (items items) (loc loc))
         (match items
           ['() (succeed (make-syntax (cons ': (reverse results-rev)) left-pos) loc)]
           [(cons item items) (>>= (walk item loc)
                                   (lambda (r loc)
                                     (loop (cons r results-rev) items loc)))]))]
      [(:char-val ci-str)
       (define i (loc->index loc))
       (define j (+ i (string-length ci-str)))
       (define head (and (<= j input-length) (bytes->string/latin-1 (subbytes input i j))))
       (if (and head (string-ci=? head ci-str))
           (succeed (make-syntax head loc) (advance-string loc head))
           (fail (format "expected ~v" ci-str) loc))]
      [(:range lo hi)
       (define i (loc->index loc))
       (define head (and (< i input-length) (bytes-ref input i)))
       (if (and head (<= lo head hi))
           (succeed (make-syntax head loc) (advance-byte loc head))
           (fail (format "input ~a out-of-range [~a-~a]" head lo hi) loc))]))
  ;; TODO: Work out a sensible reply format. Discard incomplete
  ;; parses? etc. TODO: Is it possible for an alternative parse to be
  ;; incomplete, alongside a complete alternative?
  (match (walk ast loc)
    [(list (? parse-error? e))
     e]
    [(list (? parse-error? e) (and r (parse-result _ loc)))
     (if (= (loc->index loc) input-length)
         r
         (list e r))]
    [results results]))

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
    [(parse-error msg loc)
     (printf "SYNTAX ERROR\n~a\n~a\n"
             msg
             (srcloc->string* loc))]
    [(parse-result cst loc)
     (pretty-print (abnf-cst->ast cst))]
    [other
     (printf "AMBIGUOUS RESULT\n")
     (pretty-print other)])
  )
