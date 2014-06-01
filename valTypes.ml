type 'a ring = 'a Ring.t

type composition_type = Label | Group | Formula | QuotBlock | QuotString

type quotation = composition_type * char * char

type script   = statement ring
and statement = term ring
and term =
| Compound   of quotation * term ring
| QuotedText of quotation * string
| BareText   of string
