#lang racket/base

(require racket/match)
(require (only-in racket/list append*))

(require (prefix-in : "ast.rkt"))
(require (prefix-in abnf: "boot.rkt"))

(struct parse-result (value loc) #:prefab)
(struct parse-error (message loc) #:prefab)

(define (advance-string loc s) (+ loc (string-length s)))
(define (advance-byte loc c) (+ loc 1))

(define (merge-error e1 e2)
  (if (>= (parse-error-loc e1) (parse-error-loc e2)) e1 e2))

(define (srcloc->string* loc input)
  ;; (local-require (only-in racket/string string-split))
  ;; (bytes->string/latin-1 (subbytes input 0 loc))
  ;; TODO: count line/column
  (format "~a" loc))

(define (make-syntax v loc) v)

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

(define (loc->index loc) loc)

(define (interpret env ast input source-name)
  (define input-length (bytes-length input))
  (define (walk ast loc ks kf)
    (define left-pos loc)
    (match ast
      [(:reference name)
       (define item
         (hash-ref env name (lambda () (error 'interpret "Nonexistent ABNF rule: ~v" name))))
       (walk item loc
             (lambda (r loc) (ks (make-syntax (list name r) loc) loc))
             kf)]
      [(:repetition min max item)
       (let loop ((results-rev '()) (loc loc) (count 0))
         (define item-loc loc)
         (walk item item-loc
               (lambda (r loc)
                 (let ((count (+ count 1)))
                   (if (or (not max) (<= count max))
                       (loop (cons r results-rev) loc count)
                       (kf "too many repetitions" item-loc))))
               (lambda (msg loc)
                 (if (< count min)
                     (kf msg loc)
                     (ks (make-syntax (list '* (reverse results-rev)) left-pos) item-loc)))))]
      [(:alternation items)
       (match (combine no-error
                       (for/list [(item (in-list items)) (index (in-naturals))]
                         (walk item left-pos
                               (lambda (r loc)
                                 (succeed (make-syntax (list '/ index r) left-pos) loc))
                               fail)))
         [(list (parse-error msg loc)) (kf msg loc)]
         [(list e results ...) (combine e
                                        (for/list [(r (in-list results))]
                                          (match-define (parse-result value loc) r)
                                          (ks value loc)))])]
      [(:concatenation items)
       (define left-pos loc)
       (let loop ((results-rev '()) (items items) (loc loc))
         (match items
           ['()
            (ks (make-syntax (cons ': (reverse results-rev)) left-pos) loc)]
           [(cons item items)
            (walk item loc
                  (lambda (r loc) (loop (cons r results-rev) items loc))
                  kf)]))]
      [(:char-val ci-str)
       (define i (loc->index loc))
       (define j (+ i (string-length ci-str)))
       (define head (and (<= j input-length) (bytes->string/latin-1 (subbytes input i j))))
       (if (and head (string-ci=? head ci-str))
           (ks (make-syntax head loc) (advance-string loc head))
           (kf (format "expected ~v" ci-str) loc))]
      [(:range lo hi)
       (define i (loc->index loc))
       (define head (and (< i input-length) (bytes-ref input i)))
       (if (and head (<= lo head hi))
           (ks (make-syntax head loc) (advance-byte loc head))
           (kf (format "input ~a out-of-range [~a-~a]" head lo hi) loc))]))
  ;; TODO: Work out a sensible reply format. Discard incomplete
  ;; parses? etc. TODO: Is it possible for an alternative parse to be
  ;; incomplete, alongside a complete alternative?
  (match (walk ast 0 succeed fail)
    [(list (? parse-error? e)) e]
    [(list (? parse-error? e) (and r (parse-result _ loc)))
     (if (= (loc->index loc) input-length)
         r
         (list e r))]
    [results results]))

(module+ main
  (require racket/file)
  (require racket/pretty)
  (require "abnf-semantics.rkt")
  (define-values (input source-name) (values

                                      ;; #"foo = %x20 %x20\r\n"
                                      ;; "adhoc"

                                      ;; (file->bytes "rfc5234-section-4.abnf")
                                      ;; "rfc5234-section-4.abnf"

                                      ;; (file->bytes "rfc5234-appendix-b.abnf")
                                      ;; "rfc5234-appendix-b.abnf"

                                      (file->bytes "rfc5322.abnf")
                                      "rfc5322.abnf"

                                      ))
  (match (time (interpret (:rulelist->hash abnf:rulelist)
                          (:reference 'rulelist)
                          input
                          source-name))
    [(parse-error msg loc)
     (printf "SYNTAX ERROR\n~a\n~a\n"
             msg
             (srcloc->string* loc input))]
    [(parse-result cst loc)
     (pretty-print (abnf-cst->ast cst))]
    [other
     (printf "AMBIGUOUS RESULT\n")
     (pretty-print other)])
  )
