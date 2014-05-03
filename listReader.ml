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

(* This mimics TclFindElement *)
let read_tcl_quote sc =
  let rec loop openBraces inQuotes sc =
    if SC.end_of_string sc then
      if openBraces <> 0 then
	failwith "unmatched open brace in list"
      else if inQuotes then
	failwith "unmatched open quote in list"
      else
	sc
    else match SC.peek sc with
    | '{' ->
      loop
	(if openBraces > 0 then openBraces+1 else openBraces)
	inQuotes
	(SC.get_advanced sc)
    | '}' ->
      (if openBraces = 1 then
	  (SC.get_advanced sc)
       else
	  loop
	    (if openBraces > 1 then openBraces-1 else openBraces)
	    inQuotes
	    (SC.get_advanced sc)
      )
    | '\\' ->
      (* Escape sequence is not supported. Just escaping one char. *)
      if SC.try_advance ~len:2 sc then
	loop openBraces inQuotes sc
      else
	failwith "premature end of backslash escape"
    | '"' ->
      if inQuotes then
	SC.get_advanced sc
      else
	loop openBraces inQuotes (SC.get_advanced sc)
    | _ ->
      loop openBraces inQuotes (SC.get_advanced sc)
  in
  let start = SC.pos sc in
  if SM.char '{' sc then
    SC.to_string ~start (loop 1 false sc)
  else if SM.char '"' sc then
    SC.to_string ~start (loop 0 true sc)
  else
    failwith "Invalid beginning of tcl_list"

