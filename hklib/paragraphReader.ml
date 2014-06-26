(* -*- coding: utf-8 -*- *)

open Core.Std
let (//) = Optutil.(//)

(*
  Paragraph here is text section delimited by \n\n.

  - [^\n]
  - \n (?! [^\n])
  - \n \n

  Txt: [^\n] region known not have \n\n
  Nl : \n    known position of \n
  Unk:       unknown region. next search target
  Gbg:       garbage after finish.

  Each reading loop tries to find '\n' from Unk region.
  if '\n' found, it represents next Nl' region

  - Unk starts from pos+1
  - Gbg starts from finish

*)

type t = In_buffer.t * Buffer.t

let create ?(readsize=256) ?(debug) ic =
  (In_buffer.create ~readsize ?debug ic, Buffer.create readsize)

let readbuf_contents (readbuf, _) =
  In_buffer.contents readbuf

let outbuf_contents (_, outbuf) =
  Buffer.contents outbuf

let outbuf_length (_, outbuf) =
  Buffer.length outbuf

let emit_outbuf (_, outbuf) =
  let out = Buffer.contents outbuf in
  Buffer.clear outbuf;
  out

let lfindi ?(pos=0) ?fin str ~f =
  (* core's lfindi does not support ~fin *)
  let fin = fin // (String.length str) in
  let rec loop str pos fin =
    if pos >= fin then
      None
    else if f str.[pos] then
      Some pos
    else
      loop str (pos+1) fin
  in
  loop str pos fin

let find_non_newline (readbuf, _) =
  (* Hand-inlining, sigh... I want C++ like template specialization here. *)
  let rec lfindi_non_newline pos fin str =
    if pos >= fin then
      None
    else if str.[pos] <> '\n' then
      Some pos
    else
      lfindi_non_newline (pos+1) fin str 
  in
  let rec loop readbuf =
    match In_buffer.to_range readbuf with
    | None -> false
    | Some (str, pos, fin) -> (
      let nlpos = lfindi_non_newline pos fin str in
      if In_buffer.advance_if ~opt:nlpos readbuf then
	true
      else if In_buffer.eof readbuf then
	false
      else
	(In_buffer.set_pos ~pos:fin readbuf;
	 loop readbuf)
    )
  in
  loop readbuf

let find_end_of_paragraph ?(pos=0) ~fin str =
  (* This too. *)
  let rec lfindi_newline pos fin str =
    if pos >= fin then
      None
    else if str.[pos] = '\n' then
      Some pos
    else
      lfindi_newline (pos+1) fin str
  in
  let rec loop pos fin str =
    match lfindi_newline pos fin str with
    | None -> None
    | Some nlpos ->
      (if nlpos+1 < fin && str.[nlpos+1] = '\n' then
	  Some nlpos
       else
	  loop (nlpos+1) fin str
      )
  in
  loop pos fin str
    
let read t =
  let rec loop ((readbuf, outbuf) as t) =
    (* Since find_non_newline is called before here,
       we can assume we have Some(non_newline) or None.
    *)
    match In_buffer.to_range readbuf with
    | None ->
      outbuf_length t > 0
    | Some (str, pos, fin) ->
      match find_end_of_paragraph ~pos ~fin str with
      | None -> (
	In_buffer.flush ~outbuf readbuf;
	loop t
      )
      | Some eop -> (
	In_buffer.flush ~outbuf ~fin:eop ~set_pos:(eop+2) readbuf;
	true
      )
  in
  if not (find_non_newline t) then
    None
  else if (loop t) then
    Some (emit_outbuf t)
  else
    None

