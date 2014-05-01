(* -*- coding: utf-8 -*- *)

open Core.Std

module SC = Strcursor
module SR = Strrange
type t = SR.t list

let space c = c = ' ' || c = '\t' || c = '\n'
let non_space c = not (space c)

let next_token sc =
  let start = SC.advance_while space sc in
  if not (SC.can_peek sc) then
    None
  else
    (ignore (SC.advance_while non_space sc);
     Some (SC.to_strrange ~start:start sc))

let of_string str =
  let rec loop sc lst =
      (*Printf.printf "pos %d in subj %s\n" (SC.pos sc) sc.SC.subject;*)
    match next_token sc with
    | None     -> List.rev lst
    | Some tok -> loop sc (tok :: lst)
  in
  let sc = SC.of_string str in
  loop sc []

