#lang racket/base

(provide (all-defined-out))

(struct header (names) #:prefab)
(struct record (fields) #:prefab)
