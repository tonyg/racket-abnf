#lang racket/base

(require abnf)
(require racket/match)

(define-abnf-parser parse-bools/cst "bools-rules.rkt" bools values)

(define (cst->ast/1 cst)
  (let walk ((cst cst))
    (match cst
      [`(bools (* ,vs)) (map walk vs)]
      [`(bool (/ 0 ,_)) #t]
      [`(bool (/ 1 ,_)) #f])))

(define (cst->ast/2 cst)
  (traverse (lambda (walk cst)
              (match cst
                [`(true ,_) #t]
                [`(false ,_) #f]
                [`(,_ ,v) (walk v)]))
            cst))

(module+ test
  (require rackunit)

  (define cst1 '(bools
                 (*
                  ((bool (/ 0 (true "t")))
                   (bool (/ 0 (true "t")))
                   (bool (/ 1 (false "f")))
                   (bool (/ 1 (false "f")))
                   (bool (/ 0 (true "t")))
                   (bool (/ 1 (false "f")))
                   (bool (/ 1 (false "f")))))))

  (check-equal? (parse-bools/cst "ttfftff") cst1)
  (check-equal? (cst->ast/1 cst1) (list #t #t #f #f #t #f #f))
  (check-equal? (cst->ast/2 cst1) (list #t #t #f #f #t #f #f))
  )
