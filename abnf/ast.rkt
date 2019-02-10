#lang racket/base

(provide (all-defined-out))

(struct rule (name item-promise) #:prefab)
(struct repetition (min max item) #:prefab)
(struct alternation (items) #:prefab)
(struct concatenation (items) #:prefab)
(struct char-val (ci-str) #:prefab)
(struct range (lo hi) #:prefab)
