signature MY_LIST =
sig
    exception MyListException of string (* error message *)
    type 'a myList				 
    val cons: ('a * 'a myList) -> 'a myList 
    val first: 'a myList -> 'a
    val rest: 'a myList -> 'a myList
    val null: 'a myList (* careful: same name as SML's empty list value *)
    val isNull: 'a myList -> bool
    val toList: 'a myList -> 'a list (* Convert a 'a myList to a regular 'a list *)
    val toString : ('a -> string) -> 'a myList -> string (* Expected SML list rep, e.g "[7, 2, 4]" *)
end

structure MyListSOP :> MY_LIST = 
struct

  exception MyListException of string (* error message *)
  datatype 'a myList = Null | Cons of 'a * ('a myList)

  (* flesh out the remaining functions in the signature *)

  (* This function is supplied for you *)
  fun toString eltToString elts = 
      "[" ^ (String.concatWith ","  (map eltToString (toList elts))) ^ "]"

end

(*
structure MyListSMLList :> MY_LIST = 
struct

  exception MyListException of string (* error message *)
  type 'a myList = 'a list

  (* flesh out the remaining functions in the signature *)

  (* This function is supplied for you *)
  fun toString eltToString elts = 
      "[" ^ (String.concatWith ","  (map eltToString (toList elts))) ^ "]"

end
*)

(* Testing *)

(* Uncomment this when ready *)
(* open MyListSOP; *) 
(* open MyListSMLList;*)

(*

val mil1 = cons(1, cons(2, null))
val b1 = isNull mil1
val i1 = first mil1
val mil2 = rest mil1
val mil1l = toList mil1
val mil1s = toString Int.toString mil1
val b2 = isNull mil2
val i2 = first mil2
val mil3 = rest mil2
val mil2l = toList mil2
val mil2s = toString Int.toString mil2
val b3 = isNull mil3

val msl1 = cons("a", cons("b", cons("c", null)))
val s1 = first msl1
val msl2 = rest msl1
val msl1l = toList msl1
val msl1s = toString (fn str => str) msl1
val s2 = first msl2
val msl3 = rest msl2
val msl2l = toList msl2
val msl2s = toString (fn str => str) msl2

*)



