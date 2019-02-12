#lang racket/base

(require racket/match)
(require (only-in racket/list append-map))

(require (prefix-in : "ast.rkt"))
(require (prefix-in abnf: "boot.rkt"))

(define (make-syntax val-exp loc-exp)
  val-exp)

(define (compile env ast ks kf)
  (define (walk ast ks kf)
    (define left-pos (gensym 'left-pos))
    (match ast
      [(:reference name)
       (when (not (hash-has-key? env name))
         (error 'compile "Nonexistent ABNF rule: ~v" name))
       `(,name input loc ,ks ,kf)]
      [(:repetition min max item)
       (define item-loc (gensym 'item-loc))
       (define results-rev (gensym 'results-rev))
       (define count (gensym 'count))
       `(let ((,left-pos loc))
          (let loop ((,results-rev '()) (loc loc) (,count 0))
            (define ,item-loc loc)
            ,(walk item
                   `(lambda (r loc)
                      (let ((,count (+ ,count 1)))
                        ,(if (not max)
                             `(loop (cons r ,results-rev) loc ,count)
                             `(if (<= count max)
                                  (loop (cons r ,results-rev) loc ,count)
                                  (,kf "too many repetitions" ,item-loc)))))
                   `(lambda (msg loc)
                      (if (< ,count min)
                          (,kf msg loc)
                          (,ks ,(make-syntax `(list '* (reverse ,results-rev)) left-pos)
                               ,item-loc))))))]
      [(:alternation items)
       `(let ((,left-pos loc))
          (match (combine no-error
                          (list ,@(for/list [(item (in-list items)) (index (in-naturals))]
                                    (walk item
                                          `(lambda (r loc)
                                             (succeed ,(make-syntax `(list '/ ,index r) left-pos)
                                                      loc))
                                          `fail))))
            [(list (parse-error msg loc)) (,kf msg loc)]
            [(list e results ...) (combine e
                                           (for/list [(r (in-list results))]
                                             (match-define (parse-result value loc) r)
                                             (,ks value loc)))]))]
      [(:concatenation items)
       `(let ((,left-pos loc))
          ,(let loop ((results-rev '()) (items items))
             (match items
               ['()
                `(,ks ,(make-syntax `(list ': ,@(reverse results-rev)) left-pos) loc)]
               [(cons item items)
                (walk item
                      (let ((r (gensym 'r)))
                        `(lambda (,r loc) ,(loop (cons r results-rev) items)))
                      kf)])))]
      [(:char-val ci-str)
       `(let* ((i loc)
               (j (+ i ,(string-length ci-str)))
               (head (and (<= j (bytes-length input))
                          (bytes->string/latin-1 (subbytes input i j)))))
          (if (and head (string-ci=? head ,ci-str))
              (,ks ,(make-syntax `head `loc) (+ loc ,(string-length ci-str)))
              (,kf ,(format "expected ~v" ci-str) loc)))]
      [(:range lo hi)
       `(let* ((i loc)
               (head (and (< i (bytes-length input))
                          (bytes-ref input i))))
          (if (and head (<= ,lo head ,hi))
              (,ks ,(make-syntax `head `loc) (+ loc 1))
              (,kf (format "input ~a out-of-range [~a-~a]" head ,lo ,hi) loc)))]))
  (walk ast ks kf))

(define (compile-rulelist rulelist)
  (define env (:rulelist->hash rulelist))
  `(module DUMMY racket/base
     (provide ,@(hash-keys env))
     ,@(for/list [((name ast) (in-hash env))]
         `(define (,name input loc0 ks kf)
            (let ((loc loc0))
              ,(compile env ast
                        `(lambda (r loc1) (ks ,(make-syntax `(list ',name r) `loc0) loc1))
                        `kf))))))

(module+ main
  (require racket/file)
  (require racket/pretty)
  (pretty-print (compile-rulelist abnf:rulelist)))
