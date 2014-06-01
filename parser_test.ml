(* -*- coding: utf-8 -*- *)

open OUnit2

open Core.Std
(* open Optutil *)

module CC = CharClass
module SM = Strmatch
module SC = Strcursor

module M = Parser
open M.T

let pretty_term term =
  M.Fmt.with_pp M.Fmt.pp_term term


let q_group   = (Group,   '(', ')')
and q_formula = (Formula, '$', ';')
and q_label   = (Label,   '[', ']')

let () = run_test_tt_main
  ("parser">:::
      (List.map ~f:(fun (pretty, term) ->
	("pp: "^ pretty) >::(fun _ ->
	  assert_equal
	    pretty
	    (pretty_term term)))
	 ["foo", (BareText "foo")
	 ; "(foo bar)", Compound((Group, '(', ')'), Ring.of_list
	   [BareText "foo"; BareText "bar"])
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
      ) [0, (BareText "abc"), (BareText "abc")
	; 1, (BareText "def"), (BareText "abc")
	; -1, (BareText "abc"), (BareText "def")
	; 0
	  , Compound((Group, '(', ')'), Ring.of_list [BareText "foo"])
	  , Compound((Group, '(', ')'), Ring.of_list [BareText "foo"])
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
       ) ["foo bar baz"
	     , Some(BareText "foo")

	 ; "(foo bar) baz"
	   , Some(Compound(q_group, Ring.of_list
	     [BareText "foo"; BareText "bar"]))

	 ; "$foo bar; baz"
	   , Some(Compound(q_formula, Ring.of_list
	     [BareText "foo"; BareText "bar"]))

	 ; "$foo[+ 3][* 8]; baz"
	   , Some(Compound(q_formula, Ring.of_list
	     [BareText "foo"
	     ; Compound(q_label, Ring.of_list
	       [BareText "+"; BareText "3"])
	     ; Compound(q_label, Ring.of_list
	       [BareText "*"; BareText "8"])]))

	 ; "{foo bar} baz"
	   , Some(QuotedText((QuotBlock,'{','}'), "{foo bar}"))
	 ; "{foo {bar baz}{}} baz"
	   , Some(QuotedText((QuotBlock,'{','}'), "{foo {bar baz}{}}"))
	 ; "{)(;$][} baz"
	   , Some(QuotedText((QuotBlock,'{','}'), "{)(;$][}"))

	 ; "\"foo bar\" baz"
	   , Some(QuotedText((QuotString,'"','"'), "\"foo bar\""))

	 ; "(foo (bar baz))"
	   , Some(Compound(q_group, Ring.of_list
	     [BareText "foo"; Compound(q_group, Ring.of_list
	       [BareText "bar"; BareText "baz"])]))

	 ]
      )
  )
