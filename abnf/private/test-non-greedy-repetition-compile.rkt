#lang racket/base

(module+ test
  (require rackunit)
  (require abnf)

  (define-abnf-parser greedy-S "greedy-6spaces.rkt" S values)
  (define-abnf-parser non-greedy-S/complete "non-greedy-6spaces.rkt" S values)
  (define non-greedy-S
    (abnf-parser #:incomplete-parse-error? #f
                 (let () (local-require "non-greedy-6spaces.rkt") S)
                 values))
  (define non-greedy-efstar
    (abnf-parser #:incomplete-parse-error? #f
                 (let () (local-require "non-greedy-efstar.rkt") S)
                 values))

  (check-equal?
   (greedy-S "     ")
   `(S (: (* ((SP 32) (SP 32) (SP 32) (SP 32)))
          (* ((SP 32))))))

  (check-equal?
   (non-greedy-S "     " #:on-ambiguity (lambda (_input outcomes) outcomes))
   '(#s(parse-error #s(range 32 32) 5)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32) (SP 32))) (* ((SP 32))))) 5)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32) (SP 32))) (* ()))) 4)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32))) (* ((SP 32) (SP 32))))) 5)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32))) (* ((SP 32))))) 4)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32))) (* ()))) 3)
     #s(parse-result (S (: (* ((SP 32) (SP 32))) (* ((SP 32) (SP 32))))) 4)
     #s(parse-result (S (: (* ((SP 32) (SP 32))) (* ((SP 32))))) 3)
     #s(parse-result (S (: (* ((SP 32) (SP 32))) (* ()))) 2)))

  (check-equal?
   (non-greedy-S/complete "     " #:on-ambiguity (lambda (_input outcomes) outcomes))
   '(#s(parse-error #s(range 32 32) 5)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32) (SP 32))) (* ((SP 32))))) 5)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32))) (* ((SP 32) (SP 32))))) 5)))

  (check-equal?
   (non-greedy-S "      " #:on-ambiguity (lambda (_input outcomes) outcomes))
   '(#s(parse-error () -1)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32) (SP 32))) (* ((SP 32) (SP 32))))) 6)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32) (SP 32))) (* ((SP 32))))) 5)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32) (SP 32))) (* ()))) 4)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32))) (* ((SP 32) (SP 32))))) 5)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32))) (* ((SP 32))))) 4)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32))) (* ()))) 3)
     #s(parse-result (S (: (* ((SP 32) (SP 32))) (* ((SP 32) (SP 32))))) 4)
     #s(parse-result (S (: (* ((SP 32) (SP 32))) (* ((SP 32))))) 3)
     #s(parse-result (S (: (* ((SP 32) (SP 32))) (* ()))) 2)))

  (check-equal?
   (non-greedy-S/complete "      " #:on-ambiguity (lambda (_input outcomes) outcomes))
   '(S (: (* ((SP 32) (SP 32) (SP 32) (SP 32)))
          (* ((SP 32) (SP 32))))))

  (check-equal?
   (non-greedy-efstar "efef" #:on-ambiguity (lambda (_input outcomes) outcomes))
   '(#s(parse-error #s(range 101 101) 4)
     #s(parse-result (S (* ((/ 0 101) (/ 1 102) (/ 0 101) (/ 1 102)))) 4)
     #s(parse-result (S (* ((/ 0 101) (/ 1 102) (/ 0 101)))) 3)
     #s(parse-result (S (* ((/ 0 101) (/ 1 102)))) 2)
     #s(parse-result (S (* ((/ 0 101)))) 1)
     #s(parse-result (S (* ())) 0))))
