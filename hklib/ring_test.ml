(* -*- coding: utf-8 -*- *)

open OUnit2

open Core.Std

module M = Ring

  
let () = run_test_tt_main
  ("ring">:::
      ["round trip">::
	  (fun _ ->
	    assert_equal
	      ["foo"; "bar"]
	      (Ring.to_list (Ring.of_list ["foo"; "bar"])))
      ]
   @ (List.map ~f:(fun (sum, l) ->
     "length">::(fun _ ->
       assert_equal sum (Ring.length (Ring.of_list l))
     )
   ) [0, []
     ;1, [1]
     ;3, [1;2;3]
   ])

   @ (List.map ~f:(fun (sum, l) ->
     "iter">::(fun _ ->
       let n = ref 0 in
       Ring.iter ~f:(fun v -> n := v + !n) (Ring.of_list l);
       assert_equal sum !n
     )
   ) [0, []
     ;6, [1;2;3]
   ])
   @ (List.map ~f:(fun (res, l, r) ->
     let theme = Printf.sprintf "compare %s %s => %d"
       (String.concat l) (String.concat r) res
     in
     theme >:: (fun _ ->
       assert_equal res (Ring.compare (Ring.of_list l) (Ring.of_list r))
     )
   ) [0, [], []
     ; 0, ["A"], ["A"]
     ; 1, ["B"], ["A"]
     ; -1, ["A"], ["B"]
     ; 0, ["A";"B"], ["A";"B"]
     ; 1, ["A";"C"], ["A";"B"]
     ; -1, ["A";"B"], ["A";"C"]
     ; 1, ["A";"B";"C"], ["A";"B"]
     ; -1, ["A";"B"], ["A";"B";"C"]
     ]
   )
  )
