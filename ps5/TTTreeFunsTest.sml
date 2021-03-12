(* Testing functions for TTTreeFuns *)

use "../utils/Show.sml"; (* For showing integer lists *)
use "../utils/Utils.sml"; (* For Utils.range *)
use "../utils/Shuffle.sml"; (* For shuffling testing inputs *)
use "TTTreeFuns.sml"; (* Student solutions *)

exception TreeException of int list 

fun listToTTTree xs =
  foldl (fn (x,t) => insert x t) L xs

fun makeTTTrees size =
  map (fn shuf => ((shuf, listToTTTree shuf) (* return input/output pairs *)
		   handle exn => raise (TreeException shuf)))
      (Shuffle.allShuffles size)

fun testInsertUpToSize size =
  let val inputOutputPairs = 
	  List.concat (map (fn n => (makeTTTrees n))
			   (Utils.range 0 (size + 1)))
      val (validPairs, invalidPairs) = 
	  List.partition (fn (_,t) => isValid t) inputOutputPairs
      val wrongElts = List.filter (fn (input,t) => 
				      (elts t) <> (Utils.range 0 (length input))) 
				  validPairs
  in 

      if (null invalidPairs) andalso (null wrongElts) 
     then ("Passed all " ^ (Int.toString (length inputOutputPairs)) ^ " test cases", [])
     else if (not (null invalidPairs))
          then ("FAILED TEST CASES: invalid trees in these input/output pairs", 
		Utils.takeUpTo 10 invalidPairs)
          else ("FAILED TEST CASES: elements or element order is wrong in these input/output pairs", 
		Utils.takeUpTo 10 wrongElts)
  end
  handle TreeException ints => 
	 ("UNEXPECTED TESTING PROBLEM: Exception at " ^ ((Show.list Show.int) ints), [])

(* Print deep lists and trees *)
val _ = (Control.Print.printLength := 1000;
	 Control.Print.printDepth := 1000)

val testInsertUpToSize5 = testInsertUpToSize 5
val testInsertUpToSize10 = testInsertUpToSize 10
val testInsertUpToSize25 = testInsertUpToSize 25
val testInsertUpToSize50 = testInsertUpToSize 50
val testInsertUpToSize100 = testInsertUpToSize 100
	

	
  
	
	
					
			
	   
				    



						     

