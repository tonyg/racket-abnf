#lang racket/base

(module+ main
  (require (prefix-in our: abnf/rfc8259))
  (require (prefix-in core: json))
  (require racket/file)
  (require racket/pretty)

  (require racket/runtime-path)
  (define-runtime-path here ".")

  (define (dotimes n f)
    (when (positive? n) (dotimes (- n 1) f))
    (f))

  (for [(input-file (in-list (list "example1.json"
                                   "example2.json"
                                   "example3.json"
                                   "example4.json"
                                   "example5.json"
                                   "exampleN.json")))]
    (let* ((input (file->string (build-path here input-file))))
      (newline)
      (println input-file)
      (pretty-print (void (time (dotimes 1000 (lambda () (our:string->jsexpr input))))))
      (pretty-print (void (time (dotimes 1000 (lambda () (core:string->jsexpr input)))))))))
