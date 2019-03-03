#lang racket/base

(module+ test
  (require abnf/rfc8259)
  (require rackunit)

  (check-equal? (bytes->jsexpr #"{}") (hasheq))
  (define z-water-bass-bytes #"\x7A\xE6\xB0\xB4\xF0\x9D\x84\x9E")
  (define z-water-bass-string (list->string (map integer->char '(122 27700 119070))))

  (define j-zwb-1 "\"\\u007a\\u6c34\\ud834\\udd1e\"")
  (define j-zwb-2 "\"z\\u6C34\\uD834\\uDD1E\"")
  (define j-zwb-3 "\"z\u6c34\ud834\udd1e\"")
  (define j-zwb-4 (string-append "\"" z-water-bass-string "\""))

  (check-equal? (bytes->jsexpr j-zwb-1) z-water-bass-string)
  (check-equal? (bytes->jsexpr j-zwb-2) z-water-bass-string)
  (check-equal? (bytes->jsexpr j-zwb-3) z-water-bass-string)
  (check-equal? (bytes->jsexpr j-zwb-4) z-water-bass-string)

  (check-equal? (bytes->jsexpr #"\"\\b\\n\\r\\f\\t\\\\\\\"\\/\"") "\b\n\r\f\t\\\"/"))
