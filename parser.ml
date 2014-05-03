(* -*- coding: utf-8 -*- *)
(* open Core.Std *)

include ListReader
open Optutil

module CC = CharClass

let cc_nlsem = CC.of_string "-0x0a ;"
let cc_hws   = CC.of_string "-0x20 -0x09"
let cc_ws    = CC.of_string "-0x20 -0x09 -0x0a"
let cc_atom  = CC.of_string "_ A-Z a-z 0-9 = ! @ % & * < > ? + - * / :"

type 'a ring = 'a Ring.t

type composition_type = Label | Group | Formula | QuotBlock | QuotString

type quotation = composition_type * char * char

type term =
| Compound of quotation * term ring
| Quoted   of quotation * string
| Bareword of string

let read_group quot ~body sc =
  let (_, opn, clo) = quot in
  if SM.not_char opn sc then
    None
  else
    let res = body sc in
    if SM.char clo sc then
      match res with
      | None -> None
      | Some v -> Some (Compound (quot, v))
    else
      failwith (Printf.sprintf "label not closed with '%c'" clo)

let rec _script sc =
  read_list ~sep:(SM.many cc_nlsem) ~body:_statement sc

and _statement sc =
  read_list ~sep:(SM.many cc_hws) ~body:_labeled_term sc

and _labeled_term sc =
  read_seplist ~sep:_label ~body:_term sc

and _label sc =
  read_group (Label, '[', ']') ~body:_term_or_label_list sc

and _term_or_label_list sc =
  read_list ~sep:(SM.many cc_ws) ~body:_term_or_label sc

and _term sc =
  if SC.end_of_string sc then
    None
  else
    match SC.peek sc with
    | '(' -> _group   sc
    | '$' -> _formula sc
    | '{' -> _qblock  sc
    | '"' -> _qstring sc
    | _   -> _atom    sc

and _term_or_label sc =
  (_term |// _label) sc

and _group sc =
  (read_group (Group, '(', ')') ~body:_term_or_label_list sc)

and _formula sc =
  (read_group (Formula, '$', ';') ~body:_term_or_label_list sc)

and _atom sc =
  (SM.tab ~matching:(SM.many cc_atom) sc)
  &&>> (fun v -> Bareword v)

and _qblock sc =
  Some (Quoted ((QuotBlock, '{', '}'), (read_tcl_quote sc)))

and _qstring sc =
  Some (Quoted ((QuotString, '"', '"'), (read_tcl_quote sc)))

let rec compare_term lt rt =
  match (lt, rt) with
  | Compound (lq, lr), Compound (rq, rr) when lq = rq ->
    Ring.compare ~cmp:compare_term lr rr
  | Quoted (lq, ls), Quoted (rq, rs) when lq = rq ->
    String.compare ls rs
  | Bareword ls, Bareword rs ->
    String.compare ls rs
  | _, _ ->
    failwith "type mismatch!"

let rec pp_term ppf t =
  match t with
  | Compound ((_, opn, clo), r) ->
    Format.fprintf ppf "%c%a%c" opn pp_ring clo
  | Quoted ((_, opn, clo), s) ->
    Format.fprintf ppf "%c%a%c" opn pp_str clo
  | Bareword s ->
    Format.fprintf ppf "%a" pp_str

and pp_ring ppf r =
  Ring.iter ~sep:(fun _ -> Format.fprintf ppf " ") ~f:(pp_term ppf) r

and pp_str ppf s =
  Format.fprintf ppf "%s" s

