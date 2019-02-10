#lang racket/base
;; From https://tools.ietf.org/html/rfc5234 section 4.

(provide (all-defined-out))

(require (prefix-in : "ast.rkt"))
(require (only-in racket/promise delay))

(define rulelist
  (:rule 'rulelist
         (delay
           (:repetition 1 #f
                        (:alternation (list rule
                                            (:concatenation (list (:repetition 0 #f c-wsp)
                                                                  c-nl))))))))

(define rule
  (:rule 'rule
         (delay
           (:concatenation (list rulename
                                 defined-as
                                 elements
                                 c-nl)))))
                       ; continues if next line starts
                       ;  with white space

(define rulename
  (:rule 'rulename
         (delay
           (:concatenation (list ALPHA
                                 (:repetition 0 #f
                                              (:alternation (list ALPHA
                                                                  DIGIT
                                                                  (:char-val "-")))))))))

(define defined-as
  (:rule 'defined-as
         (delay
           (:concatenation (list (:repetition 0 #f c-wsp)
                                 (:alternation (list (:char-val "=")
                                                     (:char-val "=/")))
                                 (:repetition 0 #f c-wsp))))))
                       ; basic rules definition and
                       ;  incremental alternatives

(define elements
  (:rule 'elements
         (delay
           (:concatenation (list alternation
                                 (:repetition 0 #f c-wsp))))))

(define c-wsp
  (:rule 'c-wsp
         (delay
           (:alternation (list WSP
                               (:concatenation (list c-nl WSP)))))))

(define c-nl
  (:rule 'c-nl
         (delay
           (:alternation (list comment CRLF LF)))))
                       ; comment or newline

(define comment
  (:rule 'comment
         (delay
           (:concatenation (list (:char-val ";")
                                 (:repetition 0 #f (:alternation (list WSP VCHAR)))
                                 (:alternation (list CRLF LF)))))))

(define alternation
  (:rule 'alternation
         (delay
           (:concatenation (list concatenation
                                 (:repetition 0 #f (:concatenation
                                                    (list (:repetition 0 #f c-wsp)
                                                          (:char-val "/")
                                                          (:repetition 0 #f c-wsp)
                                                          concatenation))))))))

(define concatenation
  (:rule 'concatenation
         (delay
           (:concatenation (list repetition
                                 (:repetition 0 #f (:concatenation
                                                    (list (:repetition 1 #f c-wsp)
                                                          repetition))))))))

(define repetition
  (:rule 'repetition
         (delay
           (:concatenation (list (:repetition 0 1 repeat)
                                 element)))))

(define repeat
  (:rule 'repeat
         (delay
           (:alternation (list (:repetition 1 #f DIGIT)
                               (:concatenation (list (:repetition 0 #f DIGIT)
                                                     (:char-val "*")
                                                     (:repetition 0 #f DIGIT))))))))

(define element
  (:rule 'element
         (delay
           (:alternation (list rulename
                               group
                               option
                               char-val
                               num-val
                               prose-val)))))

(define group
  (:rule 'group
         (delay
           (:concatenation (list (:char-val "(")
                                 (:repetition 0 #f c-wsp)
                                 alternation
                                 (:repetition 0 #f c-wsp)
                                 (:char-val ")"))))))

(define option
  (:rule 'option
         (delay
           (:concatenation (list (:char-val "[")
                                 (:repetition 0 #f c-wsp)
                                 alternation
                                 (:repetition 0 #f c-wsp)
                                 (:char-val "]"))))))

(define char-val
  (:rule 'char-val
         (delay
           (:concatenation (list DQUOTE
                                 (:repetition 0 #f (:alternation (list (:range #x20 #x21)
                                                                       (:range #x23 #x7e))))
                                 DQUOTE)))))
                       ; quoted string of SP and VCHAR
                       ;  without DQUOTE

(define num-val
  (:rule 'num-val
         (delay
           (:concatenation (list (:char-val "%")
                                 (:alternation (list bin-val
                                                     dec-val
                                                     hex-val)))))))

(define bin-val
  (:rule 'bin-val
         (delay
           (:concatenation
            (list (:char-val "b")
                  (:repetition 1 #f BIT)
                  (:repetition 0 1 (:alternation
                                    (list (:repetition 1 #f (:concatenation
                                                             (list (:char-val ".")
                                                                   (:repetition 1 #f BIT))))
                                          (:concatenation (list (:char-val "-")
                                                                (:repetition 1 #f BIT)))))))))))
                       ; series of concatenated bit values
                       ;  or single ONEOF range

(define dec-val
  (:rule 'dec-val
         (delay
           (:concatenation
            (list (:char-val "d")
                  (:repetition 1 #f DIGIT)
                  (:repetition 0 1 (:alternation
                                    (list (:repetition 1 #f (:concatenation
                                                             (list (:char-val ".")
                                                                   (:repetition 1 #f DIGIT))))
                                          (:concatenation (list (:char-val "-")
                                                                (:repetition 1 #f DIGIT)))))))))))

(define hex-val
  (:rule 'hex-val
         (delay
           (:concatenation
            (list (:char-val "x")
                  (:repetition 1 #f HEXDIG)
                  (:repetition 0 1 (:alternation
                                    (list (:repetition 1 #f (:concatenation
                                                             (list (:char-val ".")
                                                                   (:repetition 1 #f HEXDIG))))
                                          (:concatenation (list (:char-val "-")
                                                                (:repetition 1 #f HEXDIG)))))))))))

(define prose-val
  (:rule 'prose-val
         (delay
           (:concatenation (list (:char-val "<")
                                 (:repetition 0 #f (:alternation (list (:range #x20 #x3d)
                                                                       (:range #x3f #x7e))))
                                 (:char-val ">"))))))
                       ; bracketed string of SP and VCHAR
                       ;  without angles
                       ; prose description, to be used as
                       ;  last resort

(define ALPHA
  (:rule 'ALPHA
         (delay
           (:alternation (list (:range #x41 #x5a)
                               (:range #x61 #x7a))))))

(define BIT
  (:rule 'BIT
         (delay
           (:alternation (list (:char-val "0")
                               (:char-val "1"))))))

(define CHAR
  (:rule 'CHAR
         (delay
           (:range #x01 #x7f))))
                       ; any 7-bit US-ASCII character,
                       ;  excluding NUL

(define CR
  (:rule 'CR (delay (:range #x0d #x0d))))
                       ; carriage return

(define CRLF
  (:rule 'CRLF (delay (:concatenation (list CR LF)))))
                       ; Internet standard newline

(define CTL
  (:rule 'CTL (delay (:alternation (list (:range #x00 #x1f) (:range #x7f #x7f))))))
                       ; controls

(define DIGIT
  (:rule 'DIGIT (delay (:range #x30 #x39))))
                       ; 0-9

(define DQUOTE
  (:rule 'DQUOTE (delay (:range #x22 #x22))))
                       ; " (Double Quote)

(define HEXDIG
  (:rule 'HEXDIG (delay (:alternation (list DIGIT
                                            (:char-val "A")
                                            (:char-val "B")
                                            (:char-val "C")
                                            (:char-val "D")
                                            (:char-val "E")
                                            (:char-val "F"))))))

(define HTAB
  (:rule 'HTAB (delay (:range #x09 #x09))))
                       ; horizontal tab

(define LF
  (:rule 'LF (delay (:range #x0a #x0a))))
                       ; linefeed

(define LWSP
  (:rule 'LWSP
         (delay (:repetition 0 #f (:alternation (list WSP (:concatenation (list CRLF WSP))))))))
                       ; Use of this linear-white-space rule
                       ;  permits lines containing only white
                       ;  space that are no longer legal in
                       ;  mail headers and have caused
                       ;  interoperability problems in other
                       ;  contexts.
                       ; Do not use when defining mail
                       ;  headers and use with caution in
                       ;  other contexts.

(define OCTET
  (:rule 'OCTET (delay (:range #x00 #xff))))
                       ; 8 bits of data

(define SP
  (:rule 'SP (delay (:range #x20 #x20))))

(define VCHAR
  (:rule 'VCHAR (delay (:range #x21 #x7e))))
                       ; visible (printing) characters

(define WSP
  (:rule 'WSP (delay (:alternation (list SP HTAB)))))
                       ; white space
