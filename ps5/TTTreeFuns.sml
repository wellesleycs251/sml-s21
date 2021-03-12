(* Put your name here:    *)

(* CS 251 Fall '20 Problem Set 5 Problem 2 TTTreeFuns *)

use "TTTree.sml"; (* Given 2-3-tree datatypes and sample trees *)

(********************************** Part a ************************************)

(*----------------------------------------------------------------------------*)
(* Here, write your definition of elts: TTTree -> int list *)

(*----------------------------------------------------------------------------*)
(* Here, write your definition of isSorted: int list -> bool *)

(*----------------------------------------------------------------------------*)
(* Uncomment this definition when elts and isSorted are defined *)
(* 
fun satisfiesOrderingProperty t = isSorted(elts t)
*)

(********************************** Part b ************************************)

(*----------------------------------------------------------------------------*)
(* Uncomment and flesh out this skeleton of the function height: TTTree -> int option *)
(* 
(* height: TTTree -> int option *)
fun height L = (* return an appropriate option value here *)
  | height (W(l, _, r)) =
    (case (height(l), height(r)) of (* put appropriate pattern clauses here *))
  | (* handle the H case here, similarly to the W case *)
*)

(*----------------------------------------------------------------------------*)
(* Uncomment this definition when height is defined *)
(* 
fun satisfiesHeightProperty t = Option.isSome (height t)
*)

(*----------------------------------------------------------------------------*)
(* Uncomment this definition when satisfiesOrderingProperty and
   satisfiesHeightProperty are defined *)
(* 
fun isValid t = satisfiesOrderingProperty t andalso satisfiesHeightProperty t
*)

(********************************** Part c ************************************)

(* datatype for insertion *)
datatype InsResult =
	 Tree of TTTree
	 | KickedUp of TTTree * int * TTTree (* pseudo two-node "kicked up" from below *)

(*----------------------------------------------------------------------------*)
(* Uncomment and flesh out this skeleton of the functions insert and ins *)

(* 					  
(* insert: int -> TTTree -> TTTree *)
fun insert v t =
  case ins v t of
      Tree t => t
    | KickedUp(l, w, r) => W(l, w, r)
and (* "and" glues together mutually recursive functions *)
(* ins: int -> TTTree -> InsResult *)
    ins v L = KickedUp (L, v, L)
  | ins v (W(l, X, r)) =
    if v <= X
    then (case ins v l of
                Tree l' => Tree(W(l', X, r))
              | KickedUp (l', w, m)  => Tree(H(l', w, m, X, r)))
    else (* flesh this out *)
  | (* handle an H node similarly to the W node based on rules from the handout *)
*)

(*----------------------------------------------------------------------------*)
(* Helper functions to test insert *)

fun range lo hi =
  if lo >= hi then [] else lo :: (range (lo + 1) hi)

(* Uncomment these definitions when ready to test insert *)

(*

fun listToTTTree xs =
  foldl (fn (x,t) => insert x t) L xs

fun testInsert numVals = 
    let val inputs = range 0 numVals
	val ttt = listToTTTree inputs
	val _ = if isValid ttt then
		    print "Tree is valid.\n"
		else
		    print "***TEST FAILED: Tree is invalid!\n"
	val _ = if elts ttt = inputs then
		    print "Tree contains all inputs in expected order.\n"
		else
		    print "***TEST FAILED: Tree elements are not what is expected!\n"
    in ttt
    end
*)

				

  
	

	
  
	
	
					
			
	   
				    



						     

