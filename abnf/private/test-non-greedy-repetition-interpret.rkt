#lang racket/base

(module+ test
  (require rackunit)
  (require abnf)
  (require abnf/rfc5234/abnf)
  (require abnf/interpret)

  (check-equal?
   ((rulelist->parser
     (read-abnf-rulelist "@greedy-repetition\r\nS = 2*4SP *2SP\r\nSP = %x20\r\n")
     'S)
    (->parse-input "     "))
   '(#s(parse-error () -1)
     #s(parse-result (S (: (* ((SP 32) (SP 32) (SP 32) (SP 32))) (* ((SP 32))))) 5)))

  (check-equal?
   ((rulelist->parser
     (read-abnf-rulelist "@non-greedy-repetition\r\nS = 2*4SP *2SP\r\nSP = %x20\r\n")
     'S)
    (->parse-input "     "))
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
   ((rulelist->parser
     (read-abnf-rulelist "@non-greedy-repetition\r\nS = *(%x65 / %x66)\r\n")
     'S)
    (->parse-input "efef"))
   '(#s(parse-error #s(range 101 101) 4)
     #s(parse-result (S (* ((/ 0 101) (/ 1 102) (/ 0 101) (/ 1 102)))) 4)
     #s(parse-result (S (* ((/ 0 101) (/ 1 102) (/ 0 101)))) 3)
     #s(parse-result (S (* ((/ 0 101) (/ 1 102)))) 2)
     #s(parse-result (S (* ((/ 0 101)))) 1)
     #s(parse-result (S (* ())) 0))))
