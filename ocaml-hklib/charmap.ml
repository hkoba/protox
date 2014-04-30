(* -*- coding: utf-8 -*- *)

open Core.Std

module type CharmapElem_intf = sig
  (* Not t because I want to allow including this interface into Charmap *)
  type elem_t
  val empty : elem_t
end

module Make_Charmap(Elem : CharmapElem_intf) = struct
  type t = Elem.elem_t array
    
  module CRL = CharRangeList

  let create ?(init=Elem.empty) () =
    Array.create ~len:256 init
      
  let set_from_charrange cm ~value cr =
    for i = cr.CRL.Spec.first to cr.CRL.Spec.last do
      cm.(i) <- value
    done;
    cm

  let set_from_string cm ~value str =
    let cmdefs = CRL.of_string str in
    List.iter cmdefs ~f:(fun def -> ignore (set_from_charrange cm ~value def));
    cm

  let get cm ch = cm.(int_of_char ch)

end
