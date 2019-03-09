#lang racket

(require abnf)
(require abnf/rfc5234/core)

(define-abnf-parser parse-arithmetic/cst "arithmetic-rules.rkt" expr values)

(define (eval-cst cst)
  (match cst
    [`(expr (/ 0 (: ,x "+" ,y))) (+ (eval-cst x) (eval-cst y))]
    [`(expr (/ 1 (: ,x "-" ,y))) (- (eval-cst x) (eval-cst y))]
    [`(expr (/ 2 ,z)) (eval-cst z)]

    [`(term (/ 0 (: ,x "*" ,y))) (* (eval-cst x) (eval-cst y))]
    [`(term (/ 1 (: ,x "/" ,y))) (/ (eval-cst x) (eval-cst y))]
    [`(term (/ 2 ,z)) (eval-cst z)]

    [`(factor (/ 0 (: "(" ,z ")"))) (eval-cst z)]
    [`(factor (/ 1 ,z)) (eval-cst z)]
    [`(num (: ,_ ,digits ,_)) (string->number (text digits))]))

(struct binop (function arg0 arg1) #:prefab)

(define (cst->ast cst)
  (match cst
    [`(expr (/ 0 (: ,x "+" ,y))) (binop + (cst->ast x) (cst->ast y))]
    [`(expr (/ 1 (: ,x "-" ,y))) (binop - (cst->ast x) (cst->ast y))]
    [`(expr (/ 2 ,z)) (cst->ast z)]

    [`(term (/ 0 (: ,x "*" ,y))) (binop * (cst->ast x) (cst->ast y))]
    [`(term (/ 1 (: ,x "/" ,y))) (binop / (cst->ast x) (cst->ast y))]
    [`(term (/ 2 ,z)) (cst->ast z)]

    [`(factor (/ 0 (: "(" ,z ")"))) (cst->ast z)]
    [`(factor (/ 1 ,z)) (cst->ast z)]
    [`(num (: ,_ ,digits ,_)) (string->number (text digits))]))

(define-abnf-parser parse-arithmetic "arithmetic-rules.rkt" expr cst->ast)

(define (eval-ast ast)
  (match ast
    [(binop f x y) (f (eval-ast x) (eval-ast y))]
    [(? number? n) n]))

(module+ test
  (require rackunit)

  (check-equal?
   (parse-arithmetic/cst "10 + 20 * 30 - 40")

   '(expr (/ 0 (: (term
                   (/ 2 (factor
                         (/ 1 (num (: (* ()) (* ((DIGIT 49) (DIGIT 48))) (* ((SP (/ 0 32))))))))))
                  "+"
                  (expr
                   (/ 1 (: (term
                            (/ 0 (: (factor
                                     (/ 1 (num (: (* ((SP (/ 0 32))))
                                                  (* ((DIGIT 50)
                                                      (DIGIT 48)))
                                                  (* ((SP (/ 0 32))))))))
                                    "*"
                                    (term
                                     (/ 2 (factor
                                           (/ 1 (num (: (* ((SP (/ 0 32))))
                                                        (* ((DIGIT 51)
                                                            (DIGIT 48)))
                                                        (* ((SP (/ 0 32)))))))))))))
                           "-"
                           (expr
                            (/ 2 (term
                                  (/ 2 (factor
                                        (/ 1 (num (: (* ((SP (/ 0 32))))
                                                     (* ((DIGIT 52)
                                                         (DIGIT 48)))
                                                     (* ())))))))))))))))
   )

  (check-equal?
   (text (parse-arithmetic/cst "10 + 20 * 30 - 40"))
   "10 + 20 * 30 - 40")

  (check-equal?
   (eval-cst (parse-arithmetic/cst "10 + 20 * 30 - 40"))
   570)

  (check-equal?
   (cst->ast (parse-arithmetic/cst "10 + 20 * 30 - 40"))
   (binop + 10 (binop - (binop * 20 30) 40)))

  (check-equal?
   (eval-ast (cst->ast (parse-arithmetic/cst "10 + 20 * 30 - 40")))
   570)

  (check-equal?
   (parse-arithmetic "10 + 20 * 30 - 40")
   (binop + 10 (binop - (binop * 20 30) 40)))

  )
