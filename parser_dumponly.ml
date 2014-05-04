(* -*- coding: utf-8 -*- *)

open Core.Std
open Optutil

(*
let input_paragraph ?buf ?(len=4096) ic =
  let buf = buf // Buffer.create len in
    try Buffer.add_channel buf ic len 
*)

let () =
  if Array.length Sys.argv >= 2 then
    let ic = In_channel.create Sys.argv.(1) in
    let str = In_channel.input_all ic in
    let ppf = Format.std_formatter in
    match Parser._script (Strcursor.init str) with
    | None -> print_endline "None"
    | Some script ->
      (Parser.Fmt.pp ppf script;
       Format.pp_print_flush ppf ();
       print_endline "")
