#lang racket/base

(require racket/match)
(require bitsyntax)
(require (only-in racket/promise force))

(require (prefix-in : "ast.rkt"))
(require (prefix-in abnf: "boot.rkt"))

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

;; (define *indent* 0)
;; (define (interpret ast input loc ks kf)
;;   (printf "~a~a ~a\n"
;;           (make-string *indent* #\space)
;;           (srcloc->string* loc)
;;           (let ((s (format "~v" ast)))
;;             (if (> (string-length s) 50)
;;                 (string-append (substring s 0 50) "...")
;;                 s)))
;;   (set! *indent* (+ *indent* 2))
;;   (interpret* ast
;;               input
;;               loc
;;               (lambda (r input loc)
;;                 (printf "~a✓ ~v\n"
;;                         (make-string *indent* #\space)
;;                         r)
;;                 (set! *indent* (- *indent* 2))
;;                 (ks r input loc))
;;               (lambda (e)
;;                 (printf "~a✗ ~a ~a\n"
;;                         (make-string *indent* #\space)
;;                         (parse-error-message e)
;;                         (srcloc->string* (parse-error-loc e)))
;;                 (set! *indent* (- *indent* 2))
;;                 (kf e))))

(define (make-syntax v loc)
  ;; (match-define (srcloc f l c p s) loc)
  ;; (datum->syntax #f v (list f l c p s))
  v
  )

(define (interpret ast input loc ks kf)
  (match ast
    [(:rule name item-promise)
     (interpret (force item-promise)
                input
                loc
                (lambda (r input loc)
                  (ks (make-syntax (list name r) loc) input loc))
                kf)]
    [(:repetition min max item)
     (define left-pos loc)
     (let loop ((results-rev '()) (input input) (loc loc) (count 0))
       (define fault-pos loc)
       (interpret item
                  input
                  loc
                  (lambda (r input loc)
                    (let ((count (+ count 1)))
                      (if (or (not max) (<= count max))
                          (loop (cons r results-rev) input loc count)
                          (kf (parse-error "too many repetitions" fault-pos)))))
                  (lambda (e)
                    (if (< count min)
                        (kf (parse-error "too few repetitions" fault-pos))
                        (ks (make-syntax (list '* (reverse results-rev)) left-pos) input loc)))))]
    [(:alternation items)
     ;; TODO: parallel exploration.
     (let loop ((items items) (best-e (parse-error "syntax error" loc)) (index 0))
       (match items
         ['() (kf best-e)]
         [(cons item items) (interpret item
                                       input
                                       loc
                                       (lambda (r input loc)
                                         (ks (make-syntax (list '/ index r) loc) input loc))
                                       (lambda (e)
                                         (loop items (merge-error e best-e) (+ index 1))))]))]
    [(:concatenation items)
     (define left-pos loc)
     (let loop ((results-rev '()) (items items) (input input) (loc loc))
       (match items
         ['() (ks (make-syntax (cons ': (reverse results-rev)) left-pos) input loc)]
         [(cons item items) (interpret item
                                       input
                                       loc
                                       (lambda (r input loc)
                                         (loop (cons r results-rev) items input loc))
                                       kf)]))]
    [(:char-val ci-str)
     (define-values (head0 tail) (bit-string-split-at-or-false input (* 8 (string-length ci-str))))
     (define head (and head0 (bytes->string/latin-1 (bit-string->bytes head0))))
     (if (and head (string-ci=? head ci-str))
         (ks (make-syntax head loc) tail (advance-string loc head))
         (kf (parse-error (format "expected ~v" ci-str) loc)))]
    [(:range lo hi)
     (bit-string-case input
       ([ head (tail :: binary) ]
        (if (<= lo head hi)
            (ks (make-syntax head loc) tail (advance-byte loc head))
            (kf (parse-error (format "input ~a out-of-range [~a-~a]" head lo hi) loc))))
       (else (kf (parse-error "unexpected end of input" loc))))]))

(module+ main
  (require racket/file)
  (require racket/pretty)
  (require "abnf-semantics.rkt")
  (match (time (interpret abnf:rulelist
                          (file->bytes "rfc5234-section-4.rktd")
                          (srcloc "rfc5234-section-4.rktd" 1 0 1 #f)
                          list
                          list))
    [(list cst remaining loc)
     (pretty-print (abnf-cst->ast cst))
     (pretty-print loc)
     (pretty-print (bit-string->bytes remaining))]
    [(list err)
     (printf "SYNTAX ERROR: ~a\n" err)]))
