(* -*- coding: utf-8 -*- *)

module String = Core.Std.String
module SU = Strutil

open Optutil

type t = {subject: string; mutable pos: int}

let init ?(pos=0) subject = {subject; pos}
let of_string ?(pos=0) subject = {subject; pos}

let to_subject_pos {subject; pos} = subject, pos

let pos sc = sc.pos

let rewind ~pos sc = sc.pos <- pos; sc

let to_string ?(start=0) ?(finish) ?(finoff) sc =
    (* XXX: finoff は良くない。もっと直感的に *)
  String.sub sc.subject ~pos:start ~len:((finish // sc.pos)
					 - (finoff // 0) - start)

let to_strrange ?(start=0) {subject; pos} =
  Strrange.create ~start ~finish:pos subject

let peek ?(off=0) cursor =
  cursor.subject.[cursor.pos + off]

let length {subject; pos} =
  String.length subject - pos

let can_peek ?(len=1) cursor =
  length cursor - len >= 0

let end_of_string cursor =
  not (can_peek cursor)

let eos = end_of_string

let advance ?(len=1) cursor =
  cursor.pos <- cursor.pos + len;
  true  (* XXX: maybe bad habit *)

let get_advanced ?len cursor =
  (ignore (advance ?len cursor); cursor)

let advance_while test cursor =
  while can_peek cursor && test (peek cursor); do
    ignore (advance cursor)
  done;
  pos cursor

let compare str cursor =
  SU.compare str ~roff:cursor.pos cursor.subject

exception End_of_cursor of t

let get ?(skip=0) cursor =
  if not (can_peek ~len:(skip+1) cursor) then
    raise (End_of_cursor cursor)
  else
    (if skip > 0 then
	ignore (advance ~len:skip cursor);
     let ch = peek cursor in
     ignore (advance cursor);
     ch)

