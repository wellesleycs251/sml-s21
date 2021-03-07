datatype 'a bintree =
    Leaf
  | Node of 'a bintree * 'a * 'a bintree (* left subtree, value, right subtree *)

val intTree = Node(Node(Leaf,2,Leaf),
		    4, 
		    Node(Node(Leaf, 1, Node(Leaf, 5, Leaf)),
			 6,
			 Node(Leaf, 3, Leaf)))

val stringTree = Node(Node (Leaf,"like",Leaf),
		       "green",     
		       Node (Node (Leaf,"eggs",Leaf),
			     "and",
			     Node (Leaf,"ham",Leaf)))

(* val numNodes = fn : 'a bintree -> int
   Returns the number of nodes in a binary tree 

    - numNodes intTree;
    val it = 6 : int

    numNodes stringTree;
    val it = 5 : int 
*)
fun numNodes Leaf = 0
  | numNodes (Node(l,v,r)) = 1 + (numNodes l) + (numNodes r)

(* val height = fn : 'a bintree -> int 
   Returns the height of a binary tree

   - height intTree;
   val it = 4 : int
  
   - height stringTree;
   val it = 3 : int
 *)
fun height Leaf = ()
  | height (Node(l,v,r)) = ()

(* val sum_nodes = fn : int bintree -> int 
  Returns the sum of node values in binary tree of ints 

  - sumNodes intTree;
  val it = 21 : int
*)
fun sumNodes Leaf = ()
  | sumNodes (Node(l,v,r)) = ()

(* val inlist = fn : 'a bintree -> 'a list 
   Returns a list of the node values in in-order 

   - inlist intTree;
   val it = [2,4,1,5,6,3] : int list

    - inlist stringTree;val it = ["like","green","eggs","and","ham"] 
       : string list
*)
fun inlist Leaf = ()
  | inlist (Node(l,v,r)) = ()

(* val map_tree = fn : ('a -> 'b) -> 'a bintree -> 'b bintree 
   maps function over every node in a binary tree

   - mapTree (fn x => x*2) intTree;
   val it =  Node (Node (Leaf,4,Leaf),8,                
                   Node (Node (Leaf,2,Node (Leaf,10,Leaf)),12,
                         Node (Leaf,6,Leaf))) : int bintree

   - mapTree (fn s => String.sub(s,0)) stringTree;
   val it =  Node (Node (Leaf,#"l",Leaf),#"g",
                   Node (Node (Leaf,#"e",Leaf),#"a”,
                         Node (Leaf,#"h",Leaf))) : char bintree
*)
fun mapTree f Leaf = ()
  | mapTree f (Node(l,v,r)) = ()

(* val fold_tree = fn : ('b * 'a * 'b -> 'b) -> 'b -> 'a bintree -> 'b 
   binary tree accumulation

   - foldTree (fn (lsum,v,rsum) => lsum + v + rsum) 0 intTree;
   val it = 21 : int

   - foldTree (fn (lstr,v,rstr) => lstr ^ v ^ rstr) " " stringTree;
   val it = " like green eggs and ham " : string
*)
fun foldTree comb leafval Leaf = () 
  | foldTree comb leafval (Node(l,v,r)) = ()
    


