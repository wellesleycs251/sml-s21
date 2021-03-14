(* Binary search trees on integers *)
(* Uses the bintree datatype *)

use "bintree-soln.sml";

(* Set printLength & printDepth appropriately *)		     
Control.Print.printLength := 100;
Control.Print.printDepth := 100;

fun singleton v = Node(Leaf, v, Leaf)

fun insert x Leaf = singleton x
  | insert x (t as (Node(l,v,r))) =
    if x = v then 
      t
    else if 
      x < v then Node(insert x l, v, r)
    else (* x > v *) 
      Node(l, v, insert x r)

fun listToBst xs = foldl (fn (x,t) => insert x t) Leaf xs

fun member x Leaf = false
  | member x (Node(l,v,r)) = (x = v) 
			     orelse (x < v andalso member x l) 
			     orelse (x > v andalso  member x r)

(* Test cases *)							

val testBst = listToBst [9,4,2,7,14,12,17];

val testMember = map (fn i => (i, member i testBst)) 
		     [0,1,2,3,4,5,6,7,8,9,
		      10,11,12,13,14,15,16,17,18,19]


