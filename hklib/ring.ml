(* -*- coding: utf-8 -*- *)

open Core.Std
open Optutil

module Elem = struct
  type 'a t = {
    value: 'a;
    mutable next: 'a t;
    mutable prev: 'a t;
  }

  let init value =
    let rec x = {value; next=x; prev=x} in
    x
  let append_to x value =
    let last = x.next in
    let elt = {value; prev=x; next=last} in
    (last.prev <- elt;
     x.next <- elt; elt)

  let value elt = elt.value
  let next  elt = elt.next
  let prev  elt = elt.prev

  let is_equal x y = Pervasives.(==) x y
  let not_equal x y = Pervasives.(!=) x  y
end

open Elem

type 'a t = 'a Elem.t option ref

let create ()  = ref None
let init v     = ref (Some (Elem.init v))
let is_empty t = !t = None
let last t     = !t

let optional t =
  if is_empty t then
    None
  else
    Some t

(* XXX: should have better name... *)
let unit_append t value =
  t := Some (match last t with
  | None -> Elem.init value
  | Some old -> Elem.append_to old value
  )

let append t value =
  unit_append t value; t

let ( +: ) t v = append t v

let iter ?sep ~f t =
  match last t with
  | None -> ()
  | Some last ->
    let rec loop el =
      f el.value;
      if Elem.not_equal el last then (
	call_optionally sep;
	loop el.next
      )
    in
    loop last.next

let find ~f t =
  match last t with
  | None -> None
  | Some last ->
    let rec loop el =
      if f el.value then
	Some el.value
      else
	loop el.next
    in
    loop last.next

let compare ~cmp l' r' =
  match last l', last r' with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some llast, Some rlast ->
    let rec loop le re =
      let c = cmp le.value re.value in
      if c <> 0 then
	c
      else if Elem.is_equal re.next rlast then
	1
      else if Elem.is_equal le.next llast then
	-1
      else
	loop le.next re.next
    in
    loop llast.next rlast.next

let map_to_list ~f t =
  match last t with
  | None -> []
  | Some last ->
    let rec loop el tail =
      if Elem.is_equal el last then
	tail
      else
	loop el.prev (f el.value :: tail)
    in
    loop last.prev [f last.value]
(* XXX: Having two callsite for f is bad. *)

let to_list t =
  map_to_list t ~f:(fun x -> x)

let of_list l =
  let r = create () in
  List.iter ~f:(fun v -> unit_append r v) l;
  r;
