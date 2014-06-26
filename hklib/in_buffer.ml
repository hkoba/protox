(* -*- coding: utf-8 -*- *)

open Core.Std
let (//) = Optutil.(//)

type t = {
  ic: in_channel;
  readbuf: string;
  mutable pos: int;
  mutable finish: int;
  mutable eof: bool;
  debug: int;
}

let create ?(readsize=256) ?(debug=0) ic =
  {ic; debug;
   readbuf = String.create readsize;
   pos = 0; finish = 0; eof = false}

let is_debug t = t.debug > 0

let debuglog ~msg ?pos ?fin t =
  if t.debug >= 1 then begin
    let pos = pos // t.pos in
    let fin = fin // t.finish in
    (match t.debug with
    | 1 -> Printf.printf "[%s] pos=%d fin=%d\n" msg pos fin
    | _ -> Printf.printf "[%s] pos=%d fin=%d [%s]\n"
      msg pos fin (String.sub t.readbuf ~pos
		     ~len:(pos - fin)));
    flush stdout
  end
      
let eof t = t.eof

let rewind t =
  debuglog ~msg:"rewinding" t;
  t.pos <- 0;
  t.finish <- 0

let pump t =
  debuglog ~msg:"begin pump" t;
  if t.finish - t.pos <= 0 && t.pos > 0 then
    rewind t;
  let len = String.length t.readbuf - t.finish in
  let got = In_channel.input t.ic ~buf:t.readbuf ~pos:t.finish ~len in
  t.finish <- got + t.finish;
  if got = 0 then
    t.eof <- true;
  debuglog ~msg:"end pump" t;
  got

let set_pos ~pos t =
  assert(pos <= t.finish);  (* pos can be equal to finish. *)
  t.pos <- pos

let advance_if ~opt t =
  match opt with
  | None ->
    debuglog ~msg:"not found" t;
    false
  | Some found ->
    set_pos ~pos:found t;
    debuglog ~msg:"found and advanced" t;
    true

let test_pos ~test ~pos t =
  pos < t.finish && test t.readbuf.[pos]

let test_pumped t =
  if t.pos < t.finish then
    true
  else if t.eof then
    false
  else
    (pump t > 0)

let ensure_pumped t =
  ignore (test_pumped t)

let shift ?pos t =
  debuglog ~msg:"begin shift" t;
  let pos = pos // t.pos in
  let len = t.finish - pos in
  if len > 0 then
    String.blit ~src:t.readbuf ~src_pos:pos ~dst:t.readbuf ~dst_pos:0 ~len;
  t.pos    <- 0;
  t.finish <- len;
  debuglog ~msg:"end shift" t

let flush ?(fin) ?(set_pos) ~outbuf t =
  debuglog ~msg:"begin flush with fin" ?fin t;
  let finish = fin // t.finish in
  let set_pos = set_pos // finish in
  Buffer.add_substring outbuf t.readbuf t.pos (finish - t.pos);
  t.pos <- set_pos;
  debuglog ~msg:"end flush" t

let to_range t =
  (* This automatically pump if needed. *)
  if not (test_pumped t) then
    None
  else
    Some(t.readbuf, t.pos, t.finish)

let to_sub t =
  match to_range t with
  | None -> None
  | Some(str, pos, fin) -> Some(str, pos, fin - pos)

let contents t =
  match to_sub t with
  | None -> None
  | Some(str, pos, len) ->
    Some(String.sub ~pos ~len str)
