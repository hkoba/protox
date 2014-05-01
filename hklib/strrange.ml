(* -*- coding: utf-8 -*- *)

open Core.Std

include Optutil
module SU = Strutil

type t = {base: string; start: int; finish: int}

let create ?(start=0) ?finish ?(empty=false) str =
  {base = str; start = start;
   finish =
      if empty then start
      else min (finish // String.length str) (String.length str)}

let of_string str = create str

let to_string ?(offset=0) ?length {base; start; finish} =
  let start = start + offset in
  let length = cap_if length (finish - start) in
  String.sub base ~pos:start ~len:length

let copy ?start ?finish range =
  {range with start = start // range.start; finish = finish // range.finish}

let deep_copy {base; start; finish} =
  {base = String.copy base; start = start; finish = finish}

let length {start; finish; _} = finish - start

  (*let is_empty {start; finish; _} = start >= finish*)

let get ?(offset=0) range =
  range.base.[range.start + offset]
    
let compare ?len left right =
  SU.compare
    ~loff:left.start ~llen:(cap_if len (length left)) left.base
    ~roff:right.start ~rlen:(cap_if len (length right)) right.base

let index ?(off=0) {base; start; finish} ch =
  let s = off + start in
  match SU.index ~off:s ~len:(finish - s) base ch with
  | None -> None
  | Some pos -> Some (pos - start)

