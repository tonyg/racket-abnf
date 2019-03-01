#lang racket/base

(provide rulelist->parser
         interpreter)

(require racket/match)
(require (only-in racket/list append*))

(require (prefix-in : "rfc5234/ast.rkt"))
(require "runtime.rkt")

(define (advance-string loc s) (+ loc (string-length s)))
(define (advance-byte loc c) (+ loc 1))

(define (make-syntax v loc) v)

(define (rulelist->parser rulelist entry-rule-name)
  (interpreter (:rulelist->hash rulelist)
               (:reference entry-rule-name)))

(define ((interpreter env ast) input [loc0 0] [ks succeed] [kf fail])
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
                       (ks (make-syntax (list '* (reverse results-rev)) left-pos) item-loc))))
               (lambda (failing-ast loc)
                 (if (< count min)
                     (kf failing-ast loc)
                     (ks (make-syntax (list '* (reverse results-rev)) left-pos) item-loc)))))]
      [(:biased-choice items)
       (let loop ((items items) (index 0) (e no-error))
         (match items
           ['()
            (match e [(parse-error failing-ast loc) (kf failing-ast loc)])]
           [(cons item items)
            (walk item left-pos
                  (lambda (r loc)
                    (ks (make-syntax (list '/ index r) left-pos) loc))
                  (lambda (failing-ast loc)
                    (loop items (+ index 1) (merge-error e (parse-error failing-ast loc)))))]))]
      [(:alternation items)
       (match (combine no-error
                       (for/list [(item (in-list items)) (index (in-naturals))]
                         (walk item left-pos
                               (lambda (r loc)
                                 (succeed (make-syntax (list '/ index r) left-pos) loc))
                               fail)))
         [(list (parse-error failing-ast loc)) (kf failing-ast loc)]
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
       (define count (string-length ci-str))
       (define head (input-substring input loc count))
       (if (and head (string-ci=? head ci-str))
           (ks (make-syntax head loc) (advance-string loc head))
           (kf ast loc))]
      [(:range lo hi)
       (define head (input-byte input loc))
       (if (and head (<= lo head hi))
           (ks (make-syntax head loc) (advance-byte loc head))
           (kf ast loc))]))
  (walk ast loc0 ks kf))
