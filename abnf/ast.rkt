#lang racket/base

(provide (all-defined-out))

(require racket/match)

(struct rulelist (rules) #:prefab)
(struct rule (name extension? body) #:prefab)

(struct reference (name) #:prefab)
(struct repetition (min max item) #:prefab)
(struct alternation (items) #:prefab)
(struct concatenation (items) #:prefab)
(struct char-val (ci-str) #:prefab)
(struct range (lo hi) #:prefab)

(define (rulelist->hash rl)
  (for/fold [(h (hasheq))] [(r (in-list (rulelist-rules rl)))]
    (match r
      [(rule name #f body)
       (if (hash-has-key? h name)
           (error 'rulelist->hash "Duplicate rule: ~a" name)
           (hash-set h name body))]
      [(rule name #t body)
       (if (hash-has-key? h name)
           (hash-set h name (alternation (list (hash-ref h name) body)))
           (error 'rulelist->hash "Extension to nonexistent rule: ~a" name))])))
