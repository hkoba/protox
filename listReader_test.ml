(* -*- coding: utf-8 -*- *)

open Core.Std
open OUnit2

open Optutil

module CC = CharClass
module SM = Strmatch
module SC = Strcursor

module M = ListReader

let string_of_optlist l' =
  match l' with
  | Some l -> List.to_string ~f:ident l
  | None -> "(none)"

let cc_sp   = CC.of_string "-0x09 -0x20"

let cc_name = CC.of_string "A-Z a-z _ 0-9"

let () = run_test_tt_main
  ("listReader">:::
      ["read_list">::
	  (fun _ ->
	    assert_equal ~printer:string_of_optlist
	      (Some ["foo"; "bar"; "baz"])
	      ((M.read_list
		 ~sep:(SM.many cc_sp)
		 ~body:(SM.tab ~matching:(SM.many cc_name))
		 (SC.init "foo bar baz"))
	       &&>> Ring.to_list)
	  )])
