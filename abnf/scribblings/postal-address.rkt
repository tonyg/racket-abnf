#lang racket/base

(require abnf)
(require abnf/rfc5234/core)
(require racket/match)

(define-abnf-parser parse-postal-address/cst "postal-address-rules.rkt" postal-address values)

(struct postal-address (name street area) #:prefab)
(struct name (personal-parts last-name suffix) #:prefab)
(struct initial (str) #:prefab)
(struct street (apartment house-number name) #:prefab)
(struct area (town state zip) #:prefab)

(define (cst->ast cst)
  (traverse (lambda (walk cst)
              (match cst
                [`(postal-address (: ,n ,s ,z)) (postal-address (walk n) (walk s) (walk z))]
                [`(name-part (/ 0 (: (* ((: ,ps ,_) ...)) ,l ,s ,_)))
                 (name (map walk ps)
                       (text l)
                       (match s
                         [`(* ()) #f]
                         [`(* ((: ,_ ,s))) (text s)]))]
                [`(name-part (/ 1 (: ,p ,_)))
                 (name (walk p) #f #f)]
                [`(personal-part (/ 0 ,n)) (text n)]
                [`(personal-part (/ 1 (: (initial ,i) "."))) (initial (text i))]
                [`(street (: ,a ,h ,_ ,n ,_))
                 (street (match a
                           [`(* ()) #f]
                           [`(* ((: ,a ,_))) (text a)])
                         (text h)
                         (text n))]
                [`(zip-part (: ,t "," ,_ ,s ,_ ,z ,_))
                 (area (text t) (text s) (text z))]))
            cst))

(define-abnf-parser parse-postal-address "postal-address-rules.rkt" postal-address cst->ast)

(module+ test
  (require rackunit)

  (check-equal?
   (parse-postal-address/cst "A B. C\r\n1 D\r\nE, FF 99999\r\n")
   '(postal-address
     (: (name-part
         (/ 0 (: (* ((: (personal-part (/ 0 (first-name (* ((ALPHA (/ 0 65))))))) (SP 32))
                     (: (personal-part (/ 1 (: (initial (ALPHA (/ 0 66))) "."))) (SP 32))))
                 (last-name (* ((ALPHA (/ 0 67)))))
                 (* ())
                 (CRLF (: (CR 13) (LF 10))))))
        (street
         (: (* ())
            (house-num (* ((/ 0 (DIGIT 49)))))
            (SP 32)
            (street-name (* ((VCHAR 68))))
            (CRLF (: (CR 13) (LF 10)))))
        (zip-part
         (: (town-name (* ((/ 0 (ALPHA (/ 0 69))))))
            ","
            (SP 32)
            (state (* ((ALPHA (/ 0 70)) (ALPHA (/ 0 70)))))
            (* ((SP 32)))
            (zip-code
             (: (* ((DIGIT 57) (DIGIT 57) (DIGIT 57) (DIGIT 57) (DIGIT 57))) (* ())))
            (CRLF (: (CR 13) (LF 10))))))))

  (check-equal?
   (parse-postal-address "A B. C\r\n1 D\r\nE, FF 99999\r\n")
   (postal-address (name (list "A" (initial "B"))
                         "C"
                         #f)
                   (street #f "1" "D")
                   (area "E" "FF" "99999")))

  (check-equal?
   (parse-postal-address "Charles M. Burns\r\n1000 Mammon\r\nSpringfield, XX 99999\r\n")
   (postal-address (name (list "Charles" (initial "M"))
                         "Burns"
                         #f)
                   (street #f "1000" "Mammon")
                   (area "Springfield" "XX" "99999")))

  (check-equal?
   (parse-postal-address "Barack H. Obama II\r\n1600 Pennsylvania\r\nWashington, DC 20500\r\n"
                         #:on-ambiguity (convert-all-results cst->ast))
   (list '#s(parse-error #s(range 48 57) 59)
         (postal-address (name (list "Barack" (initial "H") "Obama")
                               "II"
                               #f)
                         (street #f "1600" "Pennsylvania")
                         (area "Washington" "DC" "20500"))
         (postal-address (name (list "Barack" (initial "H"))
                               "Obama"
                               "II")
                         (street #f "1600" "Pennsylvania")
                         (area "Washington" "DC" "20500"))))

  (check-equal?
   (parse-postal-address "John Doe\r\n3 12 Prospect\r\nSpringfield, XX 99999-4444\r\n"
                         #:on-ambiguity (convert-all-results cst->ast))
   (postal-address (name (list "John")
                         "Doe"
                         #f)
                   (street "3" "12" "Prospect")
                   (area "Springfield" "XX" "99999-4444"))))
