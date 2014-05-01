(* -*- coding: utf-8 -*- *)

open Core.Std

(** Perl5's defined-or operator *)
let (//) opt def = match opt with Some x -> x | None -> def

let (|//) f g x =
  match f x with
  | Some v -> Some v
  | None -> g x

let (&&>>) x f =
  match x with
  | Some v -> Some (f v)
  | None -> None

let call_optionally optf =
  match optf with
  | Some f -> f ()
  | None -> ()

let apply_if opt fn v =
  match opt with
  | None -> v
  | Some bound -> fn bound v
    
let cap_if opt v =
  apply_if opt min v

let assigned rvar opt =
  match opt with
  | None -> false
  | Some bound -> (rvar := bound; true)

