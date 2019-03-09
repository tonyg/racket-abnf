#lang abnf

expr   = term "+" expr / term "-" expr / term
term   = factor "*" term / factor "/" term / factor
factor = "(" expr ")" / num
num    = *SP *DIGIT *SP
SP     = %x20 / %x09 / %x0d / %x0a
DIGIT  = %x30-39
