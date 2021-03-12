(* Testing file for FunSet module *)

use "../utils/Show.sml"; (* For showing integer lists *)
use "../utils/Utils.sml"; (* For Utils.range *)
use "FunSet.sml"; (* Student solution file *)

open FunSet

val range = Utils.range 

(* Lyn sez: should really functorize the following tests. *)

(* Some small sets defined without fromList *)
val s = (delete 4 (difference (union (union (insert 1 empty)
                                            (insert 4 empty))
                              (union (insert 7 empty)
                                     (insert 4 empty)))
                  (intersection (insert 1 empty)
                                (union (insert 1 empty)
                                       (insert 6 empty)))))
val s1 = insert 17 (insert 19 (insert 23 (singleton 42)))
val s2 = insert 17 (insert 23 (insert 42 (insert 57 (insert 97 (empty)))))
val s3 = delete 19 (delete 57 (delete 85 s2))
val dupSet = insert 3 (insert 2 (insert 3 (insert 1 (insert 3 (insert 2 (empty))))))
		
val s1UnionS2 = union s1 s2
val s1IntersectionS2 = intersection s1 s2
val s1DifferenceS2 = difference s1 s2
val s2DifferenceS1 = difference s2 s1
val intersectionSmallDiffs = intersection s2DifferenceS1 s1DifferenceS2

val smallSets = [
    ("empty", empty), 
    ("empty2",fromList []),
    ("set_1_2_3", fromList [1,2,3]), (* tests for quirks in odd-length lists *)
    ("s", s), 
    ("s1", s1), 
    ("s2", s2),
    ("s3", s3),
    ("dupSet", dupSet),
    ("s1UnionS2", s1UnionS2), 
    ("s1IntersectionS2", s1IntersectionS2),
    ("s1DifferenceS2", s1DifferenceS2),
    ("s2DifferenceS1", s2DifferenceS1), 
    ("intersectionSmallDiffs", intersectionSmallDiffs), 
    ("smallDelete", delete 1 (fromList [1,2])) (* test deletion from set in which elt appears *)
]






(* Test an int pred set on numbers from 0 to 100, inclusive *)
fun intPredSetToList predSet = 
    List.filter (toPred predSet) (range 0 101)

val mod2Set = fromPred (fn x => x mod 2 = 0)
val mod3Set = fromPred (fn x => x mod 3 = 0)
val lowSet = fromList (range 0 61)
val middleSet = fromList [42, 17, 57, 23]
val highSet = fromList (range 40 101)
val smallSet = insert 17 (insert 19 (insert 23 (singleton 42)))
val smallerSet = delete 23 (delete 19 (delete 57 smallSet))


val smallSetTest = intPredSetToList(smallSet)
val smallerSetTest = intPredSetToList(smallerSet)

val mod2SetTest = intPredSetToList(mod2Set)
val mod3SetTest = intPredSetToList(mod3Set)
val mod2SetUnionMod3SetTest = intPredSetToList(union mod2Set mod3Set)
val mod2SetIntersectionMod3SetTest = intPredSetToList(intersection mod2Set mod3Set)
val mod2SetDifferenceMod3SetTest = intPredSetToList(difference mod2Set mod3Set)
val mod3SetDifferenceMod2SetTest = intPredSetToList(difference mod3Set mod2Set)


val lowSetTest = intPredSetToList(lowSet)
val middleSetTest = intPredSetToList(middleSet)
val highSetTest = intPredSetToList(highSet)

val bigIntersectionTest = 
    intPredSetToList(intersection (intersection lowSet highSet)
				  (intersection mod2Set mod3Set))
val bigDifferenceTest = 
    intPredSetToList(difference (difference lowSet highSet)
				(difference mod2Set mod3Set))

(* Solutions *)
val smallSetSoln = [17,19,23,42]

val smallerSetSoln = [17,42]

val lowSetSoln = fromList (range 0 61)
val highSet = fromList (range 40 101)

val mod2SetSoln = 
    [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,
     52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100]

val mod3SetSoln = 
    [0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,
     57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]

val mod2SetUnionMod3SetSoln = 
    [0,2,3,4,6,8,9,10,12,14,15,16,18,20,21,22,24,26,27,28,30,32,33,
     34,36,38,39,40,42,44,45,46,48,50,51,52,54,56,57,58,60,62,63,64,66,
     68,69,70,72,74,75,76,78,80,81,82,84,86,87,88,90,92,93,94,96,98,99,100]

val mod2SetIntersectionMod3SetSoln =
    [0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96] 

val mod2SetDifferenceMod3SetSoln =
  [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,
   52,56,58,62,64,68,70,74,76,80,82,86,88,92,94,98,100]

val mod3SetDifferenceMod2SetSoln =
  [3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99]

val lowSetSoln = 
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60]

val middleSetSoln = [17, 23, 42, 57]

val highSetSoln = 
    [40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,
     60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]

val bigIntersectionSoln = [42,48,54,60]

val bigDifferenceSoln =
  [0,1,3,5,6,7,9,11,12,13,15,17,18,19,21,23,24,25,27,29,30,31,33,35,36,37,39]

(* Tests *)
val testCases =
    [("smallSet", smallSetTest, smallSetSoln), 
     ("smallerSet", smallerSetTest, smallerSetSoln), 
     ("mod2Set", mod2SetTest, mod2SetSoln), 
     ("mod3Set", mod3SetTest, mod3SetSoln), 
     ("mod2SetUnionMod3Set", mod2SetUnionMod3SetTest, mod2SetUnionMod3SetSoln), 
     ("mod2SetIntersectionMod3Set", mod2SetIntersectionMod3SetTest, mod2SetIntersectionMod3SetSoln),
     ("mod2SetDifferenceMod3Set", mod2SetDifferenceMod3SetTest, mod2SetDifferenceMod3SetSoln),
     ("mod3SetDifferenceMod2Set", mod3SetDifferenceMod2SetTest, mod3SetDifferenceMod2SetSoln), 
     ("mod3SetDifferenceMod2Set", mod3SetDifferenceMod2SetTest, mod3SetDifferenceMod2SetSoln), 
     ("lowSet", lowSetTest, lowSetSoln), 
     ("middleSet", middleSetTest, middleSetSoln), 
     ("highSet", highSetTest, highSetSoln), 
     ("bigIntersection", bigIntersectionTest, bigIntersectionSoln), 
     ("bigDifference", bigDifferenceTest, bigDifferenceSoln)
    ]

datatype testResult =
	 PASSED of string * int list (* name, expected/actual *)
       | FAILED of string * int list * int list (* name, expected, actual *)

fun testOneCase ((testCaseName, actual, expected), (passes, fails)) = 
    if actual = expected then
	(PASSED(testCaseName, actual) :: passes, fails)
    else
	(passes, FAILED(testCaseName, actual, expected) :: fails)

val _ = Control.Print.printLength := 101;	 
val _ = Control.Print.stringDepth := 300

fun println s = print (s ^ "\n")

fun testAll () = List.app println (testResultLines ())

and testResultLines () = 
    let val (passes, fails) = foldr testOneCase ([], []) testCases
    in 
	if fails = [] then 
	    ["Passed all " ^ (Int.toString (length testCases)) ^ " test case"
	     ^ (if (length fails) <> 1 then "s" else "") ^ ":"]
	else 
	    ["------------------------------------------------------------", 
	     "Passed " ^ (Int.toString (length passes)) ^ " test cases:"]
	    @
	    (map (fn (PASSED(name, actual)) => 
		     (* name ^ ": expected/actual = " ^ ((Show.list Show.int) actual) *)
		     name
		   | _ => "Can't happen" (* prevents potentially confusing 
				            nonexhaustive match warning *)
		 )
		 passes)
	    @
	    ["------------------------------------------------------------", 
	     "Failed " ^ (Int.toString (length fails)) ^ " test case"
	     ^ (if (length fails) <> 1 then "s" else "") ^ ":"]
	    @ 
	    (List.concat
		 (map (fn (FAILED(name, actual, expected)) => 
			  [name ^ ":", 
			   "expected = " ^ ((Show.list Show.int) expected), 
			   "  actual = " ^ ((Show.list Show.int) actual)]
			| _ => ["Can't happen"] (* prevents potentially confusing 
						   nonexhaustive match warning *)
		      )
		      fails))
	    @
	    ["------------------------------------------------------------"]
    end

val testResults = testResultLines ()

    




