(* -*- coding: utf-8 -*- *)

open Core.Std

include Optutil
module ST = Strtok
module SR = ST.SR

module Spec = struct 
  type t = {first: int; last: int}

  exception Bad_charrange of Strrange.t
  exception Bad_charcode of int

  let ck code =
    if code < 0 || code > 255 then
      raise (Bad_charcode code)
    else
      code

  let create first last =
    {first = ck first; last = ck last}

  let of_strrange sr =
    let ord = int_of_char in
    let len = SR.length sr in
    if len = 1 then
      let n = ord (SR.get sr) in
      create n n
    else if SR.get sr = '-' then
      let pos = ref 0 in
      if not (assigned pos (SR.index ~off:1 sr '-')) then
	let n = int_of_string (SR.to_string ~offset:1 sr) in
	create n n
      else if !pos < SR.length sr then
	let min = int_of_string (SR.to_string ~offset: 1 ~length:(!pos-1) sr)
	and max = int_of_string (SR.to_string ~offset:(!pos+1) sr) in
	create min max
      else
	raise (Bad_charrange sr)
      else if len = 3 && (SR.get ~offset:1 sr) = '-' then
	create (ord (SR.get sr)) (ord (SR.get ~offset:2 sr))
      else
	(* XXX: Is this good? *)
	raise (Bad_charrange sr)
end

type t = Spec.t list

let of_string str =
  List.map ~f:Spec.of_strrange (ST.of_string str)

