#lang racket/base

(provide (all-defined-out))

(struct message (headers body) #:prefab)
(struct address (display-name spec) #:prefab)

(struct header (key value) #:prefab)
(struct from header (mailboxes) #:prefab)
(struct subject header () #:prefab)
