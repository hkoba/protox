(* -*- coding: utf-8 -*- *)

open Core.Std

include Optutil

  (** [compare left right] is an extended version of {!String.compare}
      for string regions(I hope).

      [~loff], [~roff] is where substring starts.

      [~llen], [~rlen] is length to compare.
  *)
let compare ?(loff=0) ?llen left ?(roff=0) ?rlen right =
  let cmp l r =
    if l < r then -1
    else if l > r then 1
    else 0
  in
  let rec loop left loff llen right roff rlen i len =
    if i >= len then
      cmp llen rlen
    else 
      let n = cmp left.[loff + i] right.[roff + i] in
      if n <> 0 then
	n
      else
	loop left loff llen right roff rlen
	  (i+1) len
  in
  let llen = cap_if llen (String.length left  - loff)
  and rlen = cap_if rlen (String.length right - roff) in
  loop left loff llen right roff rlen
    0 (min llen rlen)

let index ?(off=0) ?len str ch =
  let len = cap_if len (String.length str - off) in
  let rec loop pos fin str ch =
    if pos >= fin then
      None
    else if str.[pos] = ch then
      Some pos
    else
      loop (pos+1) fin str ch
  in
  loop off (off+len) str ch
