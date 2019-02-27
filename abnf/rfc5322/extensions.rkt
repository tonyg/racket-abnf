#lang abnf

@require "rules.rkt"
@require abnf/rfc5234/core-rules

message-header = (fields // obs-fields) CRLF
