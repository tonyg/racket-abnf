#lang racket/base
;; From https://tools.ietf.org/html/rfc5234 section 4.

(provide rulelist)

(require (prefix-in : "ast.rkt"))

(define-syntax-rule (define-references ref ...)
  (begin (define ref (:reference 'ref)) ...))

(define-references
  rule
  rulename
  defined-as
  elements
  c-wsp
  c-nl
  comment
  biased-choice
  alternation
  concatenation
  repetition
  repeat
  element
  group
  option
  char-val
  num-val
  bin-val
  dec-val
  hex-val
  prose-val
  meta
  ALPHA
  BIT
  CHAR
  CR
  CRLF
  CTL
  DIGIT
  DQUOTE
  HEXDIG
  HTAB
  LF
  LWSP
  OCTET
  SP
  VCHAR
  WSP)

(define rulelist
  (:rulelist
   (list
    (:rule 'rulelist
           #f
           (:repetition 1 #f
                        (:alternation (list rule
                                            meta
                                            (:concatenation (list (:repetition 0 #f c-wsp)
                                                                  c-nl))))))
    (:rule 'rule
           #f
           (:concatenation (list rulename
                                 defined-as
                                 elements
                                 c-nl)))
                       ; continues if next line starts
                       ;  with white space
    (:rule 'rulename
           #f
           (:concatenation (list ALPHA
                                 (:repetition 0 #f
                                              (:alternation (list ALPHA
                                                                  DIGIT
                                                                  (:char-val "-")))))))
    (:rule 'defined-as
           #f
           (:concatenation (list (:repetition 0 #f c-wsp)
                                 (:alternation (list (:char-val "=")
                                                     (:char-val "=/")))
                                 (:repetition 0 #f c-wsp))))
                       ; basic rules definition and
                       ;  incremental alternatives
    (:rule 'elements
           #f
           (:concatenation (list biased-choice
                                 (:repetition 0 #f c-wsp))))
    (:rule 'c-wsp
           #f
           (:alternation (list WSP
                               (:concatenation (list c-nl WSP)))))
    (:rule 'c-nl
           #f
           (:alternation (list comment CRLF LF)))
                       ; comment or newline
    (:rule 'comment
           #f
           (:concatenation (list (:alternation (list (:char-val ";") (:char-val "#")))
                                 (:repetition 0 #f (:alternation (list WSP VCHAR)))
                                 (:alternation (list CRLF LF)))))
    (:rule 'biased-choice
           #f
           (:concatenation (list alternation
                                 (:repetition 0 #f (:concatenation
                                                    (list (:repetition 0 #f c-wsp)
                                                          (:char-val "//")
                                                          (:repetition 0 #f c-wsp)
                                                          alternation))))))
    (:rule 'alternation
           #f
           (:concatenation (list concatenation
                                 (:repetition 0 #f (:concatenation
                                                    (list (:repetition 0 #f c-wsp)
                                                          (:char-val "/")
                                                          (:repetition 0 #f c-wsp)
                                                          concatenation))))))
    (:rule 'concatenation
           #f
           (:concatenation (list repetition
                                 (:repetition 0 #f (:concatenation
                                                    (list (:repetition 1 #f c-wsp)
                                                          repetition))))))
    (:rule 'repetition
           #f
           (:concatenation (list (:repetition 0 1 repeat)
                                 element)))
    (:rule 'repeat
           #f
           (:alternation (list (:repetition 1 #f DIGIT)
                               (:concatenation (list (:repetition 0 #f DIGIT)
                                                     (:char-val "*")
                                                     (:repetition 0 #f DIGIT))))))
    (:rule 'element
           #f
           (:alternation (list rulename
                               group
                               option
                               char-val
                               num-val
                               prose-val)))
    (:rule 'group
           #f
           (:concatenation (list (:char-val "(")
                                 (:repetition 0 #f c-wsp)
                                 biased-choice
                                 (:repetition 0 #f c-wsp)
                                 (:char-val ")"))))
    (:rule 'option
           #f
           (:concatenation (list (:char-val "[")
                                 (:repetition 0 #f c-wsp)
                                 biased-choice
                                 (:repetition 0 #f c-wsp)
                                 (:char-val "]"))))
    (:rule 'char-val
           #f
           (:concatenation (list DQUOTE
                                 (:repetition 0 #f (:alternation (list (:range #x20 #x21)
                                                                       (:range #x23 #x7e))))
                                 DQUOTE)))
                       ; quoted string of SP and VCHAR
                       ;  without DQUOTE
    (:rule 'num-val
           #f
           (:concatenation (list (:char-val "%")
                                 (:alternation (list bin-val
                                                     dec-val
                                                     hex-val)))))
    (:rule 'bin-val
           #f
           (:concatenation
            (list (:char-val "b")
                  (:repetition 1 #f BIT)
                  (:repetition 0 1 (:alternation
                                    (list (:repetition 1 #f (:concatenation
                                                             (list (:char-val ".")
                                                                   (:repetition 1 #f BIT))))
                                          (:concatenation (list (:char-val "-")
                                                                (:repetition 1 #f BIT)))))))))
                       ; series of concatenated bit values
                       ;  or single ONEOF range
    (:rule 'dec-val
           #f
           (:concatenation
            (list (:char-val "d")
                  (:repetition 1 #f DIGIT)
                  (:repetition 0 1 (:alternation
                                    (list (:repetition 1 #f (:concatenation
                                                             (list (:char-val ".")
                                                                   (:repetition 1 #f DIGIT))))
                                          (:concatenation (list (:char-val "-")
                                                                (:repetition 1 #f DIGIT)))))))))
    (:rule 'hex-val
           #f
           (:concatenation
            (list (:char-val "x")
                  (:repetition 1 #f HEXDIG)
                  (:repetition 0 1 (:alternation
                                    (list (:repetition 1 #f (:concatenation
                                                             (list (:char-val ".")
                                                                   (:repetition 1 #f HEXDIG))))
                                          (:concatenation (list (:char-val "-")
                                                                (:repetition 1 #f HEXDIG)))))))))
    (:rule 'prose-val
           #f
           (:concatenation (list (:char-val "<")
                                 (:repetition 0 #f (:alternation (list (:range #x20 #x3d)
                                                                       (:range #x3f #x7e))))
                                 (:char-val ">"))))
                       ; bracketed string of SP and VCHAR
                       ;  without angles
                       ; prose description, to be used as
                       ;  last resort
    (:rule 'meta
           #f
           (:concatenation (list (:char-val "@")
                                 (:repetition 0 #f (:alternation (list WSP VCHAR)))
                                 (:alternation (list CRLF LF)))))
    (:rule 'ALPHA
           #f
           (:alternation (list (:range #x41 #x5a)
                               (:range #x61 #x7a))))
    (:rule 'BIT
           #f
           (:alternation (list (:char-val "0")
                               (:char-val "1"))))
    (:rule 'CHAR
           #f
           (:range #x01 #x7f))
                       ; any 7-bit US-ASCII character,
                       ;  excluding NUL
    (:rule 'CR #f (:range #x0d #x0d))
                       ; carriage return
    (:rule 'CRLF #f (:concatenation (list CR LF)))
                       ; Internet standard newline
    (:rule 'CTL #f (:alternation (list (:range #x00 #x1f) (:range #x7f #x7f))))
                       ; controls
    (:rule 'DIGIT #f (:range #x30 #x39))
                       ; 0-9
    (:rule 'DQUOTE #f (:range #x22 #x22))
                       ; " (Double Quote)
    (:rule 'HEXDIG #f (:alternation (list DIGIT
                                          (:char-val "A")
                                          (:char-val "B")
                                          (:char-val "C")
                                          (:char-val "D")
                                          (:char-val "E")
                                          (:char-val "F"))))
    (:rule 'HTAB #f (:range #x09 #x09))
                       ; horizontal tab
    (:rule 'LF #f (:range #x0a #x0a))
                       ; linefeed
    (:rule 'LWSP #f (:repetition 0 #f (:alternation (list WSP (:concatenation (list CRLF WSP))))))
                       ; Use of this linear-white-space rule
                       ;  permits lines containing only white
                       ;  space that are no longer legal in
                       ;  mail headers and have caused
                       ;  interoperability problems in other
                       ;  contexts.
                       ; Do not use when defining mail
                       ;  headers and use with caution in
                       ;  other contexts.
    (:rule 'OCTET #f (:range #x00 #xff))
                       ; 8 bits of data
    (:rule 'SP #f (:range #x20 #x20))
    (:rule 'VCHAR #f (:range #x21 #x7e))
                       ; visible (printing) characters
    (:rule 'WSP #f (:alternation (list SP HTAB)))
                       ; white space
    )))

(module+ main
  (require racket/pretty)
  (with-output-to-file "boot.rkt"
    (lambda ()
      (printf "#lang racket\n")
      (printf ";; Generated by genboot.rkt.\n")
      (printf "(require abnf/lang)\n")
      (pretty-write (syntax->datum #`(_define-and-provide-abnf #,rulelist))))))
