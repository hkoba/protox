(* -*- coding: utf-8 -*- *)

open Core.Std

include Charmap.Make_Charmap(struct
  type elem_t = bool
  let empty = false
end)

let of_string str =
  set_from_string (create ()) ~value:true str

let invert cc =
  Array.map ~f:(fun b -> not b) cc

let contains = get

