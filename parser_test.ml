(* -*- coding: utf-8 -*- *)

open OUnit2

open Core.Std
open Optutil

module CC = CharClass
module SM = Strmatch
module SC = Strcursor

module M = Parser

let pretty_term term =
  M.Fmt.with_pp M.Fmt.pp_term term


let () = run_test_tt_main
  ("parser">:::
      (List.map ~f:(fun (pretty, term) ->
	("pp: "^ pretty) >::(fun _ ->
	  assert_equal
	    pretty
	    (pretty_term term)))
	 ["foo", (M.Bareword "foo")
	 ; "(foo bar)", (M.Compound
			   ((M.Group, '(', ')')
			       , (Ring.of_list
				    [M.Bareword "foo"
				    ; M.Bareword "bar"])))
	 ]
      )
   @
     (List.map ~f:(fun (res, x, y) ->
       let theme = Printf.sprintf "compare %s %s => %d"
	 (pretty_term x) (pretty_term y) res
       in
       theme >:: (fun _ ->
	 assert_equal res (M.compare_term x y)
       )
      ) [0, (M.Bareword "abc"), (M.Bareword "abc")
	; 1, (M.Bareword "def"), (M.Bareword "abc")
	; -1, (M.Bareword "abc"), (M.Bareword "def")
	; 0
	  , (M.Compound
	       ((M.Group, '(', ')'), (Ring.of_list [M.Bareword "foo"])))
	  , (M.Compound
	       ((M.Group, '(', ')'), (Ring.of_list [M.Bareword "foo"])))
      ]
     )
   @
      (List.map ~f:(fun (input, exp) ->
	("term: " ^ input) >::(fun _ ->
	  assert_equal
	    ~cmp:(fun l r -> (Option.compare ~cmp:M.compare_term l r) = 0)
	    ~printer:(fun v -> match v with
	    | None -> "None"
	    | Some v -> M.Fmt.with_pp M.Fmt.pp_term v)
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
  )
