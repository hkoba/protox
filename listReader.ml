(* -*- coding: utf-8 -*- *)

open Core.Std

module SC = Strcursor
module SM = Strmatch

let read_list ?(debug=false) ~sep ~body sc =
  let rec loop ~sep ~body ring sc =
    ignore (sep sc);
    let start = SC.pos sc in
    match body sc with
    | None   -> ring
    | Some m -> begin
      (if debug then
	  Printf.printf "match = '%s'\n" (SC.to_string ~start sc));
      Ring.unit_append ring m;
      if SC.can_peek sc then
	loop ~sep ~body ring sc
      else
	ring
    end
  in
  Ring.optional (loop ~sep ~body (Ring.create ()) sc)

let read_seplist ~sep ~body sc =
  let rec skip_sep ring sc =
    match sep sc with
    | None -> ()
    | Some m -> (Ring.unit_append ring m; skip_sep ring sc)
  in
  let rec loop ~sep ~body ring sc =
    skip_sep ring sc;
    match body sc with
    | None   -> ring
    | Some m -> begin
      Ring.unit_append ring m;
      if SC.can_peek sc then
	loop ~sep ~body ring sc
      else
	ring
    end
  in
  Ring.optional (loop ~sep ~body (Ring.create ()) sc)

let read_group opn clo ~body sc =
  if SM.not_char opn sc then
    None
  else
    let res = body sc in
    if SM.char clo sc then
      res
    else
      failwith (Printf.sprintf "label not closed with '%c'" clo)

