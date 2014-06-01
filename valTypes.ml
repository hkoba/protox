type 'a ring = 'a Ring.t

type composition_t = Label | Group | Formula | QuotBlock | QuotString

type quotation_t = composition_t * char * char

type script_t    = statement_t ring

and  statement_t = term_t ring

and  term_t =
| Compound   of quotation_t * term_t ring
| QuotedText of quotation_t * string
| BareText   of string

