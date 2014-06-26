(* -*- coding: utf-8 -*- *)

open Core.Std
open Optutil

let () =
  let parse_n_print s =
    let ppf = Format.std_formatter in
    match Parser._script (Strcursor.init s) with
    | None -> print_endline "None"
    | Some script ->
      (Parser.Fmt.pp ppf script;
       Format.pp_print_flush ppf ();
       print_endline "")
  in
  if Array.length Sys.argv >= 2 then
    let ic = In_channel.create Sys.argv.(1) in
    let str = In_channel.input_all ic in
    parse_n_print str
  else
    let module R = ParagraphReader in
    let reader = R.create In_channel.stdin in
    let sr = ref "" in
    while (assigned sr (R.read reader)) do
      parse_n_print !sr
    done
