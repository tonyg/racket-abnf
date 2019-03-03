#lang racket/base

(provide compile-rulelist)

(require racket/match)
(require (only-in racket/list append-map))

(require (for-template racket/base))
(require (for-template racket/match))
(require (for-template "runtime.rkt"))

(require (prefix-in : "rfc5234/ast.rkt"))

(define (make-syntax val-exp loc-exp)
  val-exp)

(define (symf f . args) (string->symbol (apply format f args)))
(define (internal-rule-id name) (symf "%%~a*" name))
(define (external-rule-id name) (symf "%%~a" name))

(define (compile env ast ks kf)
  (define (walk ast ks kf)
    (define left-pos (gensym 'left-pos))
    (match ast
      [(:reference name)
       `(,(internal-rule-id name) input loc ,ks ,kf)]
      [(:repetition #t min max item)
       (define item-loc (gensym 'item-loc))
       (define results-rev (gensym 'results-rev))
       (define count (gensym 'count))
       (define loop (gensym 'repetition-loop))
       (define continue (gensym 'repetition-continue))
       `(let ((,left-pos loc)
              (,continue (lambda (r loc) (,ks r loc))))
          (let ,loop ((,results-rev '()) (loc loc) (,count 0))
            (define ,item-loc loc)
            (define (,continue)
              (,ks ,(make-syntax `(list '* (reverse ,results-rev)) left-pos) ,item-loc))
            ,(walk item
                   `(lambda (r loc)
                      (let ((,count (+ ,count 1)))
                        ,(if (not max)
                             `(,loop (cons r ,results-rev) loc ,count)
                             `(if (<= ,count ,max)
                                  (,loop (cons r ,results-rev) loc ,count)
                                  (,continue)))))
                   `(lambda (failing-ast loc)
                      ,(if (zero? min)
                           `(,continue)
                           `(if (< ,count ,min)
                                (,kf failing-ast loc)
                                (,continue)))))))]
      [(:repetition #f min max item)
       (define item-loc (gensym 'item-loc))
       (define results-rev (gensym 'results-rev))
       (define count (gensym 'count))
       (define loop (gensym 'repetition-loop))
       `(let ((,left-pos loc))
          (parallel-walk
           (list
            (let ,loop ((,results-rev '()) (loc loc) (,count 0))
              (define ,item-loc loc)
              ,(let ((more-exp (walk item
                                     `(lambda (r loc) (,loop (cons r ,results-rev) loc (+ ,count 1)))
                                     `fail)))
                 `(let ((more ,(if (not max)
                                   more-exp
                                   `(if (< ,count ,max)
                                        ,more-exp
                                        (list no-error)))))
                    (if ,(if (not max)
                             `(>= ,count ,min)
                             `(and (>= ,count ,min) (<= ,count ,max)))
                        (parallel-walk
                         (list more
                               (succeed ,(make-syntax `(list '* (reverse ,results-rev)) left-pos)
                                        ,item-loc))
                         succeed
                         fail)
                        more)))))
           (lambda (r loc) (,ks r loc))
           (lambda (failing-ast loc) (,kf failing-ast loc))))]
      [(:biased-choice items)
       (define err (gensym 'err))
       `(let ((,left-pos loc) (,err no-error))
          ,(let loop ((items items) (index 0))
             (match items
               ['()
                `(match ,err [(parse-error failing-ast loc) (,kf failing-ast loc)])]
               [(cons item items)
                (walk item
                      `(lambda (r loc)
                         (,ks ,(make-syntax `(list '/ ,index r) left-pos) loc))
                      `(lambda (failing-ast loc)
                         (let* ((,err (merge-error ,err (parse-error failing-ast loc)))
                                (loc ,left-pos))
                           ,(loop items (+ index 1)))))])))]
      [(:alternation items)
       `(let ((,left-pos loc))
          (parallel-walk (list ,@(for/list [(item (in-list items)) (index (in-naturals))]
                                   (walk item
                                         `(lambda (r loc)
                                            (succeed ,(make-syntax `(list '/ ,index r) left-pos)
                                                     loc))
                                         `fail)))
                         (lambda (r loc) (,ks r loc))
                         (lambda (failing-ast loc) (,kf failing-ast loc))))]
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
       (define count (string-length ci-str))
       `(let ((head (input-substring input loc ,count)))
          (if (and head (string-ci=? head ,ci-str))
              (,ks ,(make-syntax `head `loc) (+ loc ,count))
              (,kf ',ast loc)))]
      [(:range lo hi)
       `(let ((head (input-codepoint input loc)))
          (if (and head (<= ,lo head ,hi))
              (,ks ,(make-syntax `head `loc) (+ loc 1))
              (,kf ',ast loc)))]))
  (walk ast ks kf))

(define (compile-rulelist rulelist)
  (define env (:rulelist->hash rulelist))
  #`(begin
      #,@(for/list [(m (in-list (:rulelist->metas rulelist)))]
           (match m
             [`(require ,@spec)
              `(require ,@(for/list [(s spec)] `(prefix-in %% ,s)))]
             [other
              other]))
      (define %rulelist '#,rulelist)
      (module+ rulelist (provide %rulelist))
      (provide #,@(for/list [(n (in-hash-keys env))]
                    #`(rename-out [#,(external-rule-id n) #,n])))
      (provide #,@(for/list [(n (in-hash-keys env))]
                    #`(rename-out [#,(internal-rule-id n) #,(symf "~a*" n)])))
      #,@(for/list [(name (in-hash-keys env))]
           #`(define (#,(external-rule-id name) input)
               (#,(internal-rule-id name) input 0 succeed fail)))
      #,@(for/list [((name ast) (in-hash env))]
           #`(define (#,(internal-rule-id name) input loc0 ks kf)
               ;; (define cache-entry (input-cache-ref input loc0))
               ;; (cond
               ;;   [(not cache-entry) (error '#,name "Cycle in ABNF grammar")]
               ;;   [(box? cache-entry)
                  (define loc loc0)
                  ;; (define saved (unbox *nonterminal-stack*))
                  ;; (set-box! *nonterminal-stack* (cons (cons '#,name loc) saved))
                  ;; (printf "\n  ~a ~a ~a"
                  ;;         (current-milliseconds)
                  ;;         loc
                  ;;         (reverse (unbox *nonterminal-stack*)))
                  ;; (flush-output)
                  #,(compile env ast
                             #`(lambda (r loc1)
                                 ;; (printf " -- yes ~a/~a=~v" '#,name loc0 (input-char input loc0))
                                 ;; (set-box! *nonterminal-stack* saved)
                                 (define r1 #,(make-syntax #`(list '#,name r) #`loc0))
                                 ;; (input-cache-add! cache-entry (parse-result r1 loc1))
                                 (ks r1 loc1))
                             #`(lambda (failing-ast loc1)
                                 ;; (printf " -- no ~a/~a=~v" '#,name loc0 (input-char input loc0))
                                 ;; (set-box! *nonterminal-stack* saved)
                                 ;; (input-cache-add! cache-entry (parse-error failing-ast loc1))
                                 (kf failing-ast loc1)))
                 ;;  ]
                 ;; [else
                 ;;  (combine no-error
                 ;;           (for/list [(item (in-list cache-entry))]
                 ;;             (match item
                 ;;               [(parse-error failing-ast loc) (kf failing-ast loc)]
                 ;;               [(parse-result r loc) (ks r loc)])))])
               ))))
