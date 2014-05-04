(* -*- coding: utf-8 -*- *)
open Core.Std

module SC = Strcursor
module CC = CharClass

let char ch sc =
  SC.can_peek sc && SC.peek sc = ch && SC.advance sc
let not_char ch sc =
  not (char ch sc)

let string string sc =
  SC.compare string sc = 0
  && SC.advance ~len:(String.length string) sc
let not_string str sc =
  not (string str sc)

let can_advance_and_match cc sc =
  SC.can_peek sc
  && CC.contains cc (SC.peek sc)

let one cc sc =
  can_advance_and_match cc sc
  && SC.advance sc
let not_one cc sc =
  not (one cc sc)
    
let many cc sc =
  let nmatch = ref 0 in
  while can_advance_and_match cc sc do
    ignore (SC.advance sc); incr nmatch
  done;
  !nmatch > 0

let not_many cc sc =
  not (many cc sc)

let tab ~matching sc =
  let start = SC.pos sc in
  if matching sc then
    Some (SC.to_string ~start sc)
  else
    None

