(* -*- coding: utf-8 -*- *)

open OUnit2

(* open Core.Std open Optutil *)

module CC = CharClass
module SM = Strmatch
module SC = Strcursor

module M = Parser

let () = run_test_tt_main
  ("parser">:::
      ["term">::
	  (fun _ ->
	    assert_equal
	      (Some (M.Bareword "foo"))
	      (M._term (SC.init "foo bar baz"))
	  )
      ]
  )



