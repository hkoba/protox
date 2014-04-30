(* -*- coding: utf-8 -*- *)
(* open Core.Std *)

include ListReader
open Optutil

module CC = CharClass

let cc_nlsem = CC.of_string "-0x0a ;"
let cc_hws   = CC.of_string "-0x20 -0x09"
let cc_ws   = CC.of_string "-0x20 -0x09 -0x0a"
let cc_atom  = CC.of_string "_ A-Z a-z 0-9 = ! @ % & * < > ? + - * / :"

type 'a ring = 'a Ring.t

type (*script      = statement ring
       and statement    = labeled_term ring
       and labeled_term = term_or_label ring
       and *) term_or_label =
| Label   of term_or_label ring
| QBlock  of term_or_label ring
| Group   of term_or_label ring
| Formula of term_or_label ring
| Atom    of string

let rec _script sc =
  read_list ~sep:(SM.many cc_nlsem) ~body:_statement sc

and _statement sc =
  read_list ~debug:true ~sep:(SM.many cc_hws) ~body:_labeled_term sc

and _labeled_term sc =
  read_seplist ~sep:_label ~body:_term sc

and _label sc =
  (read_group '[' ']' ~body:_term_or_label_list sc)
  &&>> (fun v -> Some (Label v))

and _term_or_label_list sc =
  read_list ~sep:(SM.many cc_ws) ~body:_term_or_label sc

and _term sc =
  if SC.end_of_string sc then
    None
  else
    match SC.peek sc with
    | '{' -> _qblock  sc
    | '(' -> _group   sc
    | '$' -> _formula sc
    | _   -> _atom    sc

and _term_or_label sc =
  (_term |// _label) sc

and _qblock sc =
  (read_group '{' '}' ~body:_term_or_label_list sc)
  &&>> (fun v -> Some (QBlock v))

and _group sc =
  (read_group '(' ')' ~body:_term_or_label_list sc)
  &&>> (fun v -> Some (Group v))

and _formula sc =
  (read_group '$' ';' ~body:_term_or_label_list sc)
  &&>> (fun v -> Some (Formula v))

and _atom sc =
  (SM.tab ~matching:(SM.many cc_atom) sc)
  &&>> (fun v -> Some (Atom v))

let rec pp_atom ppf v =
  Format.fprintf ppf "%s" v
and pp_ring ppf v =
  Ring.iter ~sep:(fun _ -> Format.fprintf ppf " ") ~f:(pp_lterm ppf) v
and pp_lterm ppf v =
  match v with
  | QBlock ring  -> Format.fprintf ppf "{%a}" pp_ring ring
  | Group ring   -> Format.fprintf ppf "(%a)" pp_ring ring
  | Formula ring -> Format.fprintf ppf "$%a;" pp_ring ring
  | Label ring   -> Format.fprintf ppf "[%a]" pp_ring ring
  | Atom str     -> Format.fprintf ppf "%a"   pp_atom str


