(* This file has six function definitions with numerous syntax and type
   bugs. Correct the bugs so that it typechecks without error when loaded 
   into SML and the functions work as expected *)

(* 
    val sumListRec = fn : int list -> int
    sumListRec returns the sum of all ints in a list of ints.

    - sumListRec [5,2,4];
    val it = 11 : int
*)
fun sumListRec [] = 0
  | sumListRec (n, ns) = n + (sumList ns)
   
(* 
    val prodListRec = fn : int list -> int
    prodListRec returns the product of all ints in a list of ints.

    - prodListRec [5,2,4];
    val it = 40 : int
*)
fun prodListRec [] = 1
  | prodListRec n::ns  = n * prodListRec ns

(*
    val myZip = fn : 'a list * 'b list -> ('a * 'b) list
    myZip should act like ListPairs.zip. 

    - myZip([1,2,3,4],["a","b","c"]);
    val it = [(1,"a"),(2,"b"),(3,"c")] : (int * string) list

    - myZip([1,2,3],["a","b","c","d"]);
    val it = [(1,"a"),(2,"b"),(3,"c")] : (int * string) list
*)
fun myZip (xs, []) = []
  | myZip ([], ys) = []
  | myZip (x::xs, y::ys) = [x,y]::(myZip(xs,ys))

(*
    val mapCons = fn : 'a -> 'a list list -> 'a list list

    Given a list of lists LOL, mapCons v LOL returns a new
    list of lists in which v is consed onto the front of
    every list in LOL

    - mapCons 8 [[1,2,3], [4], [], [6,7]];
    val it = [[8,1,2,3],[8,4],[8],[8,6,7]] : int list list
*)
fun mapCons x yss = 
    map (fn ys => x @ ys)
	yss

(*
    val sumListFoldr = fn : int list -> int
    sumListFoldr returns the sum of all ints in a list of ints.

    - sumListFoldr [5,2,4];
    val it = 11 : int
*)
fun sumListFoldr ns = 
    foldr(fn n sum => n+sum, 
	  0,
	  ns)

(*
    val dotProduct = fn : int list -> int list -> int

    dotProduct returns the sum of the products of 
    corresponding ints in two lists of ints

    - dotProduct [2,3,5] [8,4,6];
    val it = 58 : int (* 2*8 + 3*4 + 5*6 = 16 + 12 + 30 = 58 *)
*)
fun dotProduct xs ys = 
    foldr +
	  0
	  (map (fn [x,y] => x*y)
	       ListPair.zip(xs,ys))


