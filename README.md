# racket-abnf

Racket ABNF implementation (interpreter & compiler), for directly
cutting-and-pasting from RFCs.

## Bugs

 - The error reporting is poor. Sometimes the position is off-by-one
   (?); frequently, the error message is bad. Taking strategies from
   packrat parser implementations might be a good idea?

 - Extensions "rule =/ ..." need to flatten the resulting alternations
   to a single rule-toplevel alternation, rather than a nested tree.
