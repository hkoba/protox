(* -*- coding: utf-8 -*- *)

open OUnit2

module M = Ring

  
let () = run_test_tt_main
  ("ring">:::
      ["round trip">::
	  (fun _ ->
	    assert_equal
	      ["foo"; "bar"]
	      (Ring.to_list (Ring.of_list ["foo"; "bar"])))
      ]
  )
