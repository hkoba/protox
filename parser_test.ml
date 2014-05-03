(* -*- coding: utf-8 -*- *)

open OUnit2

open Core.Std
open Optutil

module CC = CharClass
module SM = Strmatch
module SC = Strcursor

module M = Parser


let () = run_test_tt_main
  ("parser">:::
      List.map ~f:(fun (input, exp) ->
	("term: " ^ input) >::(fun _ ->
	  assert_equal
	    ~cmp:(fun l r -> (Option.compare ~cmp:M.compare_term l r) = 0)
	    ~printer:M.pp_term
	    exp
	    (M._term (SC.init input))
	  )
      ) ["foo bar baz", (Some (M.Bareword "foo"))
	; "(foo bar) baz", (Some (M.Compound
				    ((M.Group, '(', ')')
					, (Ring.of_list
					     [M.Bareword "foo"
					     ; M.Bareword "bar"]))))
      ]
  )
