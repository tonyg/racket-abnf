#lang racket/base

(provide rulelist->parser
         interpreter)

(require racket/match)
(require (only-in racket/list append*))

(require (prefix-in : "ast.rkt"))
(require "runtime.rkt")

(define (advance-string loc s) (+ loc (string-length s)))
(define (advance-byte loc c) (+ loc 1))

(define (make-syntax v loc) v)

(define (loc->index loc) loc)

(define (rulelist->parser rulelist entry-rule-name)
  (interpreter (:rulelist->hash rulelist)
               (:reference entry-rule-name)))

(define ((interpreter env ast) input [loc0 0] [ks succeed] [kf fail])
  (define input-length (bytes-length input))
  (define (walk ast loc ks kf)
    (define left-pos loc)
    (match ast
      [(:reference name)
       (define item
         (hash-ref env name (lambda () (error 'interpreter "Nonexistent ABNF rule: ~v" name))))
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
      [(:biased-choice items)
       (let loop ((items items) (index 0) (e no-error))
         (match items
           ['()
            (match e [(parse-error msg loc) (kf msg loc)])]
           [(cons item items)
            (walk item left-pos
                  (lambda (r loc)
                    (ks (make-syntax (list '/ index r) left-pos) loc))
                  (lambda (msg loc)
                    (loop items (+ index 1) (merge-error e (parse-error msg loc)))))]))]
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
           (kf (format "input ~v out-of-range [~v-~v]"
                       (if head (integer->char head) eof)
                       (integer->char lo)
                       (integer->char hi)) loc))]))
  (walk ast loc0 ks kf))

(module+ main
  (require racket/file)
  (require racket/pretty)
  (require (prefix-in abnf: "genboot.rkt"))
  (require "abnf-semantics.rkt")
  (define-values (input source-name) (values

                                      ;; #"foo = %x20 %x20\r\n"
                                      ;; "adhoc"

                                      ;; (file->bytes "rfc5234-section-4.rkt")
                                      ;; "rfc5234-section-4.rkt"

                                      ;; (file->bytes "rfc5234-appendix-b.rkt")
                                      ;; "rfc5234-appendix-b.rkt"

                                      (file->bytes "rfc5322.rkt")
                                      "rfc5322.rkt"

                                      ))
  (analyze-parser-results ((rulelist->parser abnf:rulelist 'rulelist) input)
                          input
                          source-name
                          (lambda (cst)
                            (pretty-print (abnf-cst->ast cst)))
                          (lambda (msg loc)
                            (printf "SYNTAX ERROR\n~a\n~a\n" msg (srcloc->string loc)))
                          (lambda (other)
                            (printf "AMBIGUOUS RESULT\n")
                            (pretty-print other))))
