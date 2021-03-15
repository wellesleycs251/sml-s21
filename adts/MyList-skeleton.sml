exception Unimplemented (* Placeholder during development. *)

(**********************************************************************
 MY_LIST signature
 **********************************************************************)
signature MY_LIST =
sig
    exception MyListException of string (* error message *)
    type 'a myList				 
    val cons: ('a * 'a myList) -> 'a myList 
    val first: 'a myList -> 'a
    val rest: 'a myList -> 'a myList
    val null: 'a myList (* careful: same name as SML's empty list function *)
    val isNull: 'a myList -> bool
    val toList: 'a myList -> 'a list (* convert 'a myList to regular SML 'a list *)
    val fromList: 'a list -> 'a myList (* convert regular SML 'a list to 'a myList *)
    val toString : ('a -> string) -> 'a myList -> string 
                   (* expected SML list rep as string, e.g "[7, 2, 4]".
                      ('a -> string) must be provided to convert each element to string;
                      unlike Java, Python, JavaScript, etc., SML doesn't "know" how to do this *)

end

(**********************************************************************
 MyListSOP structure: 
 internally represent 'a myList as a recursive sum-of-product datatype
 **********************************************************************)


structure MyListSOP :> MY_LIST = 
struct

  exception MyListException of string (* error message *)

  (* represent 'a myList as a recursive sum-of-product datatype *)
  datatype 'a myList = Null | Cons of 'a * ('a myList)
  
  fun cons(fst,rst) = raise Unimplemented

  fun first Null = raise MyListException "first of empty myList"
    | first (Cons(fst,_)) = fst

  fun rest _ = raise Unimplemented

  val null = Null 

  fun isNull _ = raise Unimplemented

  fun toList _ = raise Unimplemented

  fun fromList _ = raise Unimplemented

  fun toString eltToString elts = 
      "[" ^ (String.concatWith ","  (map eltToString (toList elts))) ^ "]"

end

(**********************************************************************
 MyListSML structure: 
 internally represent 'a myList as a regular SML 'a list
 **********************************************************************)

structure MyListSML :> MY_LIST = 
struct

  exception MyListException of string (* error message *)

  (* internally represent 'a myList as a regular SML 'a list *)
  type 'a myList = 'a list
  
  fun cons(fst,rst) = raise Unimplemented
  
  fun first [] = raise MyListException "first of empty myList"
    | first (fst::_) = fst

  fun rest _ = raise Unimplemented

  val null = []

  fun isNull _ = raise Unimplemented

  fun toList _ = raise Unimplemented
  fun fromList _ = raise Unimplemented

  fun toString eltToString elts = 
      "[" ^ (String.concatWith ","  (map eltToString (toList elts))) ^ "]"

end


(**********************************************************************
 Testing 
 **********************************************************************)

(* Uncomment exacly *one* of the following `open`s when ready *)
(* open MyListSOP; *)
(* open MyListSML; *)

(* Uncomment the following tests when ready. 
   The results should be exactly the same no matter
   which of the three structures is opened. 
 *)

(*

(* Tests on integer lists *)
val mil1 = cons(1, cons(2, null))
val b1 = isNull mil1
val i1 = first mil1
val mil1l = toList mil1
val mil1s = toString Int.toString mil1
val mil2 = rest mil1
val b2 = isNull mil2
val i2 = first mil2
val mil2l = toList mil2
val mil2s = toString Int.toString mil2
val mil3 = rest mil2
val b3 = isNull mil3
val mil3l = toList mil3
val mil3s = toString Int.toString mil3

(* Tests on string lists *)
fun quote(str) = "'" ^ str ^ "'" (* wrap single quotes around a string *)
val msl1 = cons("a", cons("b", cons("c", null)))
val s1 = first msl1
val msl2 = rest msl1
val msl1l = toList msl1
val msl1s = toString quote msl1
val s2 = first msl2
val msl3 = rest msl2
val msl2l = toList msl2
val msl2s = toString quote msl2

(* Conversion tests using fromList & toList *)

(* Express Racket & Python's range in terms of List.tabulate *)
fun range lo hi = List.tabulate(hi-lo, fn i => lo+i)
val milfl = toList(fromList (range 0 10))
val mslfl = toList(fromList (map Int.toString (range 0 10)))

(* myZip function to test zipping two myLists *)
fun myZip(ml1,ml2) = 
    (* Note: no pattern matching on 'a myList values! *)
    if isNull ml1 orelse isNull ml2 then 
      null
    else
      cons( (first ml1, first ml2) , 
	    myZip(rest ml1, rest ml2))

val mz1 = toList(myZip(mil1, msl1))
val mz2 = toList(myZip(cons(3, cons(4, mil1)), msl1))
   
*)



