(* Binary search trees on integers *)
(* Uses the bintree datatype *)

use "bintree-soln.sml"; 

(* Set printLength & printDepth appropriately *)		     
Control.Print.printLength := 100;
Control.Print.printDepth := 100;

fun singleton v = Node(Leaf, v, Leaf)

fun insert x Leaf = singleton x (* replace this stub *)
  | insert x (t as (Node(l,v,r))) =
    if x = v then
      t (* set semantics; allow at most one copy of v in tree *) 
    else if x < v then
      Leaf (* replace this stub *)
    else
      Leaf (* replace this stub *)

fun listToBst xs = (* Hint: use foldl *)
    Leaf (* replace this stub *)

fun member x Leaf = false
  | member x (Node(l,v,r)) = false (* replace this stub *)
    
(* Test cases *)

val testBst = listToBst [9,4,2,7,14,12,17];

val testMember = map (fn i => (i, member i testBst)) 
		     [0,1,2,3,4,5,6,7,8,9,
		      10,11,12,13,14,15,16,17,18,19]

