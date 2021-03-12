(* Testing file for OperationTreeSet module *)

use "../utils/Show.sml"; (* For showing integer lists *)
use "../utils/Utils.sml"; (* For Utils.range *)
use "OperationTreeSet.sml"; (* Student solution file *)

signature OPERATION_TREE_SET_TEST = sig
  (* empty signature to hide everything but stuff printed by side effect *)
end

structure OperationTreeSetTest :> OPERATION_TREE_SET_TEST = 

struct

open OperationTreeSet
open Show 

val range = Utils.range 
(* val printIntList = (Show.list Show.int) *)

(* Lyn sez: should really functorize the following tests. *)

(* Some small sets defined without fromList *)
val s = fn () => (delete 4 (difference (union (union (insert 1 empty)
						     (insert 4 empty))
					      (union (insert 7 empty)
						     (insert 4 empty)))
				       (intersection (insert 1 empty)
						     (union (insert 1 empty)
							    (insert 6 empty)))))
val s1 = fn () => insert 17 (insert 19 (insert 23 (singleton 42)))
val s2 = fn () => insert 17 (insert 23 (insert 42 (insert 57 (insert 97 (empty)))))
val s3 = fn () => delete 19 (delete 57 (delete 85 (s2 ())))
val dupSet = fn () => insert 3 (insert 2 (insert 3 (insert 1 (insert 3 (insert 2 (empty))))))
		
val s1UnionS2 = fn () => union (s1 ()) (s2 ())
val s1IntersectionS2 = fn () => intersection (s1 ()) (s2 ())
val s1DifferenceS2 = fn () => difference (s1 ()) (s2 ())
val s2DifferenceS1 = fn () => difference (s2 ()) (s1 ())
val intersectionSmallDiffs = fn () => intersection (s2DifferenceS1 ()) (s1DifferenceS2 ())

val tinySetThunks = [
    ("empty", fn () => empty), 
    ("singleton7", fn () => singleton 7),
    ("singleton42", fn () => singleton 42)
]

val smallSetThunks = [
    ("empty", fn () => empty), 
    ("empty2", fn () => fromList []),
    ("singleton7", fn () => singleton 7),
    ("singleton42", fn () => singleton 42),
    ("set_1_2_3", fn () => fromList [1,2,3]), (* tests for quirks in odd-length lists *)
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
    ("smallDelete", fn() => delete 1 (fromList [1,2])) (* test deletion from set in which elt appears *)
]

val lowSet = fn () => fromList (range 0 61)
val highSet = fn () => fromList (range 40 101)
val lowUnionHighSet = fn () => union (lowSet ()) (highSet ())
val lowInsersectionHighSet = fn () => intersection (lowSet ()) (highSet ())
val lowDifferenceHighSet = fn () => difference (lowSet ()) (highSet ())
val highDifferenceLowSet = fn () => difference (highSet ()) (lowSet ())
val intersectionLowHighDiffsSet = fn () => intersection (lowDifferenceHighSet ()) (highDifferenceLowSet ())
val mod2Set = fn () => fromList (List.filter (fn x => x mod 2 = 0) (range 0 101))
val mod3Set = fn () => fromList (List.filter (fn x => x mod 3 = 0) (range 0 101))
val mod2UnionMod3Set = fn () => union (mod2Set ()) (mod3Set ())
val mod2IntersectionMod3Set = fn () => intersection (mod2Set ()) (mod3Set ())
val mod2DifferenceMod3Set = fn () => difference (mod2Set ()) (mod3Set ())
val mod3DifferenceMod2Set = fn () => difference (mod3Set ()) (mod2Set ())
val intersectionModDiffsSet = fn () => intersection (mod2DifferenceMod3Set ()) (mod3DifferenceMod2Set ())
val bigIntersectionSet = fn () => intersection (intersection (lowSet ()) (highSet()))
					       (intersection (mod2Set ()) (mod3Set ()))
val bigDifferenceSet = fn () => difference (difference (lowSet ()) (highSet ()))
  					   (difference (mod2Set ()) (mod3Set ()))

val bigSetThunks = [
    ("lowSet", lowSet),
    ("highSet", highSet),
    ("lowUnionHighSet", lowUnionHighSet),
    ("lowInsersectionHighSet", lowInsersectionHighSet),
    ("lowDifferenceHighSet", lowDifferenceHighSet),
    ("highDifferenceLowSet", highDifferenceLowSet),
    ("intersectionLowHighDiffsSet", intersectionLowHighDiffsSet),
    ("mod2Set", mod2Set),
    ("mod3Set", mod3Set),
    ("mod2UnionMod3Set", mod2UnionMod3Set),
    ("mod2IntersectionMod3Set", mod2IntersectionMod3Set),
    ("mod2DifferenceMod3Set", mod2DifferenceMod3Set),
    ("intersectionModDiffsSet", intersectionModDiffsSet),
    ("mod3DifferenceMod2Set", mod3DifferenceMod2Set),
    ("bigIntersectionSet", bigIntersectionSet),
    ("bigDifferenceSet", bigDifferenceSet)
]

(* Tests of other operations on small sets *)
fun mapNamedSetThunk f namedSetThunks = map (fn (name,setThunk) => (name, fn () => (f (setThunk())))) namedSetThunks
fun testMember set = List.filter (fn i => member i set) (range 0 101)
                     handle exn => (print ("***ERROR**: " ^ (exnName exn) ^ " " ^ (exnMessage exn)); [])
fun testPred set = List.filter (toPred set) (range 0 101)
                   handle exn => (print ("***ERROR**: " ^ (exnName exn) ^ " " ^ (exnMessage exn)); [])

val tinyMembers = mapNamedSetThunk testMember tinySetThunks   
val tinyLists = mapNamedSetThunk toList tinySetThunks		    		    
val tinyIsEmpties = mapNamedSetThunk isEmpty tinySetThunks		    
val tinySizes = mapNamedSetThunk size tinySetThunks
val tinyPreds = mapNamedSetThunk testPred tinySetThunks
val tinyStrings = mapNamedSetThunk (fn s => toString Int.toString s) tinySetThunks
			       
val smallMembers = mapNamedSetThunk testMember smallSetThunks   
val smallLists = mapNamedSetThunk toList smallSetThunks		    		    
val smallIsEmpties = mapNamedSetThunk isEmpty smallSetThunks		    
val smallSizes = mapNamedSetThunk size smallSetThunks
val smallPreds = mapNamedSetThunk testPred smallSetThunks
val smallStrings = mapNamedSetThunk (fn s => toString Int.toString s) smallSetThunks

(* Tests involving "big" sets defined with fromList *)

fun intersectionTree(min, rangeLo, rangeHi, max, diff) =
    if rangeLo < min orelse rangeHi > max then
	fromList (range (Int.max(min,rangeLo)) (Int.min(rangeHi, max)))
    else intersection (intersectionTree(min, rangeLo - diff, rangeHi, max, diff))
		      (intersectionTree(min, rangeLo, rangeHi + diff, max, diff))

val bigMembers = mapNamedSetThunk testMember bigSetThunks		    		    
val bigLists = mapNamedSetThunk toList bigSetThunks		    		    
val bigIsEmpties = mapNamedSetThunk isEmpty bigSetThunks		    
val bigSizes = mapNamedSetThunk size bigSetThunks
val bigPreds = mapNamedSetThunk testPred bigSetThunks
val bigStrings = mapNamedSetThunk (fn s => toString Int.toString s) bigSetThunks

val tinyMembersSoln = [
    ("empty",[]),
    ("singleton7",[7]),
    ("singleton42",[42])
]

val tinyListsSoln = [
    ("empty",[]),
    ("singleton7",[7]),
    ("singleton42",[42])
]

val tinyIsEmptiesSoln = [
    ("empty",true),
    ("singleton7",false),
    ("singleton42",false)
]

val tinySizesSoln = [
    ("empty",0),
    ("singleton7",1),
    ("singleton42",1)
]

val tinyPredsSoln = [
    ("empty",[]),
    ("singleton7",[7]),
    ("singleton42",[42])
]

val tinyStringsSoln = [
    ("empty","{}"),
    ("singleton7","{7}"),
    ("singleton42","{42}")
]

val smallMembersSoln =
  [
    ("empty",[]),
    ("empty2",[]),
    ("singleton7",[7]),
    ("singleton42",[42]),
   ("set_1_2_3",[1,2,3]), 
   ("s",[7]),
   ("s1",[17,19,23,42]),
   ("s2",[17,23,42,57,97]),
   ("s3",[17,23,42,97]),
   ("dupSet",[1,2,3]),
   ("s1UnionS2",[17,19,23,42,57,97]),
   ("s1IntersectionS2",[17,23,42]),
   ("s1DifferenceS2",[19]),
   ("s2DifferenceS1",[57,97]),
   ("intersectionSmallDiffs",[]),
   ("smallDelete",[2])
  ]


val smallListsSoln =
  [
   ("empty",[]),
   ("empty2",[]),
   ("singleton7",[7]),
   ("singleton42",[42]),
   ("set_1_2_3",[1,2,3]), 
   ("s",[7]),
   ("s1",[17,19,23,42]),
   ("s2",[17,23,42,57,97]),
   ("s3",[17,23,42,97]),
   ("dupSet",[1,3,2]),
   ("s1UnionS2",[19,17,23,42,57,97]),
   ("s1IntersectionS2",[17,23,42]),
   ("s1DifferenceS2",[19]),
   ("s2DifferenceS1",[57,97]),
   ("intersectionSmallDiffs",[]), 
   ("smallDelete",[2])
  ]

val smallIsEmptiesSoln =
  [
   ("empty",true),
   ("empty2",true),
   ("singleton7",false),
   ("singleton42",false),
   ("set_1_2_3",false), 
   ("s",false),
   ("s1",false),
   ("s2",false),
   ("s3",false),
   ("dupSet",false),
   ("s1UnionS2",false),
   ("s1IntersectionS2",false),
   ("s1DifferenceS2",false),
   ("s2DifferenceS1",false),
   ("intersectionSmallDiffs",true),
   ("smallDelete",false)
  ]

val smallSizesSoln =
  [
   ("empty",0),
   ("empty2",0),
   ("singleton7",1),
   ("singleton42",1),
   ("set_1_2_3",3), 
   ("s",1),
   ("s1",4),
   ("s2",5),
   ("s3",4),
   ("dupSet",3),
   ("s1UnionS2",6),
   ("s1IntersectionS2",3),
   ("s1DifferenceS2",1),
   ("s2DifferenceS1",2),
   ("intersectionSmallDiffs",0),
   ("smallDelete",1)
   ]

val smallPredsSoln =
  [
   ("empty",[]),
   ("empty2",[]),
   ("singleton7",[7]),
   ("singleton42",[42]),
   ("set_1_2_3",[1,2,3]), 
   ("s",[7]),
   ("s1",[17,19,23,42]),
   ("s2",[17,23,42,57,97]),
   ("s3",[17,23,42,97]),
   ("dupSet",[1,2,3]),
   ("s1UnionS2",[17,19,23,42,57,97]),
   ("s1IntersectionS2",[17,23,42]),
   ("s1DifferenceS2",[19]),
   ("s2DifferenceS1",[57,97]),
   ("intersectionSmallDiffs",[]),
   ("smallDelete",[2])
]

val smallStringsSoln =
  [
   ("empty","{}"),
   ("empty2","{}"),
   ("singleton7","{7}"),
   ("singleton42","{42}"),
   ("set_1_2_3","{1,2,3}"), 
   ("s","{7}"),
   ("s1","{42,23,19,17}"),
   ("s2","{97,57,42,23,17}"),
   ("s3","{97,42,23,17}"),
   ("dupSet","{2,3,1}"),
   ("s1UnionS2","{97,57,42,23,17,19}"),
   ("s1IntersectionS2","{42,23,17}"),
   ("s1DifferenceS2","{19}"),
   ("s2DifferenceS1","{97,57}"),
   ("intersectionSmallDiffs","{}"),
   ("smallDelete","{2}")
] 

val bigMembersSoln =
  [("lowSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     51,52,53,54,55,56,57,58,59,60]),
   ("highSet",
    [40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
     64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,
     88,89,90,91,92,93,94,95,96,97,98,99,100]),
   ("lowUnionHighSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,
     75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,
     99,100]),
   ("lowInsersectionHighSet",
    [40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60]),
   ("lowDifferenceHighSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39]),
   ("highDifferenceLowSet",
    [61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,
     85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]),
   ("intersectionLowHighDiffsSet",[]),
   ("mod2Set",
    [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,
     52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,
     100]),
   ("mod3Set",
    [0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,
     75,78,81,84,87,90,93,96,99]),
   ("mod2UnionMod3Set",
    [0,2,3,4,6,8,9,10,12,14,15,16,18,20,21,22,24,26,27,28,30,32,33,34,36,38,
     39,40,42,44,45,46,48,50,51,52,54,56,57,58,60,62,63,64,66,68,69,70,72,74,
     75,76,78,80,81,82,84,86,87,88,90,92,93,94,96,98,99,100]),
   ("mod2IntersectionMod3Set",
    [0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96]),
   ("mod2DifferenceMod3Set",
    [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,52,56,58,62,64,68,70,74,
     76,80,82,86,88,92,94,98,100]),("intersectionModDiffsSet",[]),
   ("mod3DifferenceMod2Set",
    [3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99]),
   ("bigIntersectionSet",[42,48,54,60]),
   ("bigDifferenceSet",
    [0,1,3,5,6,7,9,11,12,13,15,17,18,19,21,23,24,25,27,29,30,31,33,35,36,37,
     39])
   ]

val bigListsSoln =
  [("lowSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     51,52,53,54,55,56,57,58,59,60]),
   ("highSet",
    [40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
     64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,
     88,89,90,91,92,93,94,95,96,97,98,99,100]),
   ("lowUnionHighSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,
     75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,
     99,100]),
   ("lowInsersectionHighSet",
    [40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60]),
   ("lowDifferenceHighSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39]),
   ("highDifferenceLowSet",
    [61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,
     85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]),
   ("intersectionLowHighDiffsSet",[]),
   ("mod2Set",
    [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,
     52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,
     100]),
   ("mod3Set",
    [0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,
     75,78,81,84,87,90,93,96,99]),
   ("mod2UnionMod3Set",
    [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,52,56,58,62,64,68,70,74,
     76,80,82,86,88,92,94,98,100,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,
     48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]),
   ("mod2IntersectionMod3Set",
    [0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96]),
   ("mod2DifferenceMod3Set",
    [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,52,56,58,62,64,68,70,74,
     76,80,82,86,88,92,94,98,100]),("intersectionModDiffsSet",[]),
   ("mod3DifferenceMod2Set",
    [3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99]),
   ("bigIntersectionSet",[42,48,54,60]),
   ("bigDifferenceSet",
    [0,1,3,5,6,7,9,11,12,13,15,17,18,19,21,23,24,25,27,29,30,31,33,35,36,37,
     39])
  ]

val bigIsEmptiesSoln =
  [("lowSet",false),
   ("highSet",false),
   ("lowUnionHighSet",false),
   ("lowInsersectionHighSet",false),
   ("lowDifferenceHighSet",false),
   ("highDifferenceLowSet",false),
   ("intersectionLowHighDiffsSet",true),
   ("mod2Set",false),
   ("mod3Set",false),
   ("mod2UnionMod3Set",false),
   ("mod2IntersectionMod3Set",false),
   ("mod2DifferenceMod3Set",false),
   ("intersectionModDiffsSet",true),
   ("mod3DifferenceMod2Set",false),
   ("bigIntersectionSet",false),
   ("bigDifferenceSet",false)
  ]

val bigSizesSoln =
  [("lowSet",61),
   ("highSet",61),
   ("lowUnionHighSet",101),
   ("lowInsersectionHighSet",21),
   ("lowDifferenceHighSet",40),
   ("highDifferenceLowSet",40),
   ("intersectionLowHighDiffsSet",0),
   ("mod2Set",51),
   ("mod3Set",34),
   ("mod2UnionMod3Set",68),
   ("mod2IntersectionMod3Set",17),
   ("mod2DifferenceMod3Set",34),
   ("intersectionModDiffsSet",0),
   ("mod3DifferenceMod2Set",17),
   ("bigIntersectionSet",4),
   ("bigDifferenceSet",27)
  ] 

val bigPredsSoln =
  [("lowSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     51,52,53,54,55,56,57,58,59,60]),
   ("highSet",
    [40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
     64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,
     88,89,90,91,92,93,94,95,96,97,98,99,100]),
   ("lowUnionHighSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,
     75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,
     99,100]),
   ("lowInsersectionHighSet",
    [40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60]),
   ("lowDifferenceHighSet",
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     27,28,29,30,31,32,33,34,35,36,37,38,39]),
   ("highDifferenceLowSet",
    [61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,
     85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]),
   ("intersectionLowHighDiffsSet",[]),
   ("mod2Set",
    [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,
     52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,
     100]),
   ("mod3Set",
    [0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,
     75,78,81,84,87,90,93,96,99]),
   ("mod2UnionMod3Set",
    [0,2,3,4,6,8,9,10,12,14,15,16,18,20,21,22,24,26,27,28,30,32,33,34,36,38,
     39,40,42,44,45,46,48,50,51,52,54,56,57,58,60,62,63,64,66,68,69,70,72,74,
     75,76,78,80,81,82,84,86,87,88,90,92,93,94,96,98,99,100]),
   ("mod2IntersectionMod3Set",
    [0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96]),
   ("mod2DifferenceMod3Set",
    [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,52,56,58,62,64,68,70,74,
     76,80,82,86,88,92,94,98,100]),("intersectionModDiffsSet",[]),
   ("mod3DifferenceMod2Set",
    [3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99]),
   ("bigIntersectionSet",[42,48,54,60]),
   ("bigDifferenceSet",
    [0,1,3,5,6,7,9,11,12,13,15,17,18,19,21,23,24,25,27,29,30,31,33,35,36,37,
     39])
  ]

val bigStringsSoln =
  [("lowSet",
    "{60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0}"),
   ("highSet",
    "{100,99,98,97,96,95,94,93,92,91,90,89,88,87,86,85,84,83,82,81,80,79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40}"),
   ("lowUnionHighSet",
    "{100,99,98,97,96,95,94,93,92,91,90,89,88,87,86,85,84,83,82,81,80,79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0}"),
   ("lowInsersectionHighSet",
    "{60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40}"),
   ("lowDifferenceHighSet",
    "{39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0}"),
   ("highDifferenceLowSet",
    "{100,99,98,97,96,95,94,93,92,91,90,89,88,87,86,85,84,83,82,81,80,79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61}"),
   ("intersectionLowHighDiffsSet","{}"),
   ("mod2Set",
    "{100,98,96,94,92,90,88,86,84,82,80,78,76,74,72,70,68,66,64,62,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,18,16,14,12,10,8,6,4,2,0}"),
   ("mod3Set",
    "{99,96,93,90,87,84,81,78,75,72,69,66,63,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3,0}"),
   ("mod2UnionMod3Set",
    "{99,96,93,90,87,84,81,78,75,72,69,66,63,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3,0,100,98,94,92,88,86,82,80,76,74,70,68,64,62,58,56,52,50,46,44,40,38,34,32,28,26,22,20,16,14,10,8,4,2}"),
   ("mod2IntersectionMod3Set",
    "{96,90,84,78,72,66,60,54,48,42,36,30,24,18,12,6,0}"),
   ("mod2DifferenceMod3Set",
    "{100,98,94,92,88,86,82,80,76,74,70,68,64,62,58,56,52,50,46,44,40,38,34,32,28,26,22,20,16,14,10,8,4,2}"),
   ("intersectionModDiffsSet","{}"),
   ("mod3DifferenceMod2Set",
    "{99,93,87,81,75,69,63,57,51,45,39,33,27,21,15,9,3}"),
   ("bigIntersectionSet","{60,54,48,42}"),
   ("bigDifferenceSet",
    "{39,37,36,35,33,31,30,29,27,25,24,23,21,19,18,17,15,13,12,11,9,7,6,5,3,1,0}")
  ]

val intersectionSetThunk = fn () => intersectionTree(0, 45, 55, 100, 10)

val extraSetThunks = [("intersectionSet", intersectionSetThunk)]

val extraMembers = mapNamedSetThunk testMember extraSetThunks		    		    
val extraLists = mapNamedSetThunk toList extraSetThunks		    		    
val extraIsEmpties = mapNamedSetThunk isEmpty extraSetThunks		    
val extraSizes = mapNamedSetThunk size extraSetThunks
val extraPreds = mapNamedSetThunk testPred extraSetThunks
val extraStrings = mapNamedSetThunk (fn s => toString Int.toString s) extraSetThunks

val extraMembersSoln = [
   ("intersectionSet",
    [45,46,47,48,49,50,51,52,53,54])
]

val extraListsSoln = [
   ("intersectionSet",
    [45,46,47,48,49,50,51,52,53,54])
]

val extraIsEmptiesSoln = [
   ("intersectionSet", false)
]

val extraSizesSoln = [
   ("intersectionSet", 10)
]

val extraPredsSoln = [
   ("intersectionSet",
    [45,46,47,48,49,50,51,52,53,54])
]

val extraStringsSoln = [
   ("intersectionSet",
    "{45,46,47,48,49,50,51,52,53,54}")
]

fun mapNamedTuples f namedTuples = 
    List.map (fn (name, valu) => (name, f valu)
		                 handle exn => (print ("***ERROR in " ^ name ^ "**: " ^ (exnName exn) ^ " " ^ (exnMessage exn) ^ "\n"); raise exn))
	     namedTuples
    handle exn => (print ("***ERROR**: " ^ (exnName exn) ^ " " ^ (exnMessage exn) ^ "\n"); [])

fun mapNamedThunkTuples f namedThunkTuples = 
    List.map (fn (name, thunk) => 
		 let fun newThunk () = 
			 let val valu = thunk ()
			 in f valu
			 end
	                 handle exn => (print ("***ERROR in " ^ name ^ "**: " ^ (exnName exn) ^ " " ^ (exnMessage exn) ^ "\n"); raise exn)
		 in (name, newThunk)
		 end)
	     namedThunkTuples

fun boolToIntList false = [0]
  | boolToIntList true = [1]

fun intToIntList i = [i]

fun sortIntList ints = ListMergeSort.sort op> ints 

(* Many students omit curlies, and code that assume they're there
   gives rise to exceptions that confuse testing. So correctly handle
   case where there are no curlies!  *)

fun removeCurlies setString = 
    let val noInitialCurly = 
	    if setString = "" orelse setString = "{" then
		""
	    else if String.isPrefix "{" setString then
		String.extract(setString, 1, NONE)
	    else 
		setString
	in let val sizeMinus1 = String.size(noInitialCurly)-1
	       val noFinalCurly = 
		   if noInitialCurly = "" orelse noInitialCurly = "}" then
		       ""
		   else if String.isSuffix "}" noInitialCurly then
		       String.substring(noInitialCurly, 0, sizeMinus1)
		   else
		       noInitialCurly
	   in noFinalCurly
	   end
	end

fun intSetStringToSortedList sep intSetString = 
    let val sepChar = String.sub(sep, 0) 
	val noCurlies = removeCurlies intSetString
	val intStrings = String.tokens (fn c => c = sepChar) noCurlies
	val unsortedInts = List.map (fn s => Option.getOpt(Int.fromString s, 0)) intStrings
	val sortedInts = sortIntList unsortedInts
    in sortedInts
    end

fun canonicalizeSetString setString =
    let val hasCurlies = String.isPrefix "{" setString orelse String.isSuffix "}" setString
	val sep = if String.isSubstring "," setString then 
		      ","
		  else if String.isSubstring " " setString then 
		      " "
		  else
		      "" (* kludge *)
    in if sep = "" then
	   setString (* If not standard separator, return unchanged *)
       else
	   let val sortedInts = intSetStringToSortedList sep setString 
	       val separatedSortedIntsString = String.concatWith sep (map int sortedInts)
	   in if hasCurlies then
		  "{" ^ separatedSortedIntsString ^ "}"
	      else
		  separatedSortedIntsString
	   end
    end

datatype test = IntListTest of string * (string * (unit -> int list)) list  * (string * int list) list
	      | BoolTest of string * (string * (unit -> bool)) list * (string * bool) list
	      | IntTest of string * (string * (unit -> int)) list * (string * int) list
	      | StringTest of string * (string * (unit -> string)) list * (string * string) list
	
val testCases =
    [

     IntListTest("tinyMembers", tinyMembers, tinyMembersSoln),
     IntListTest("tinyLists", tinyLists, tinyListsSoln),
     BoolTest("tinyIsEmpties", tinyIsEmpties, tinyIsEmptiesSoln),
     IntTest("tinySizes", tinySizes, tinySizesSoln),
     IntListTest("tinyPreds", tinyPreds, tinyPredsSoln),
     StringTest("tinyStrings", tinyStrings, tinyStringsSoln),

     IntListTest("smallMembers", smallMembers, smallMembersSoln),
     IntListTest("smallLists", 
		 mapNamedThunkTuples sortIntList smallLists, 
		 mapNamedTuples sortIntList smallListsSoln),
     BoolTest("smallIsEmpties", smallIsEmpties, smallIsEmptiesSoln),
     IntTest("smallSizes", smallSizes, smallSizesSoln),
     IntListTest("smallPreds", smallPreds, smallPredsSoln), 
     StringTest("smallStrings", 
		mapNamedThunkTuples canonicalizeSetString smallStrings,
		mapNamedTuples canonicalizeSetString smallStringsSoln),

     IntListTest("bigMembers", bigMembers, bigMembersSoln), 
     IntListTest("bigLists", 
		 mapNamedThunkTuples sortIntList bigLists, 
		 mapNamedTuples sortIntList bigListsSoln), 
     BoolTest("bigIsEmpties", bigIsEmpties, bigIsEmptiesSoln),
     IntTest("bigSizes", bigSizes, bigSizesSoln),
     IntListTest("bigPreds", bigPreds, bigPredsSoln),
     StringTest("bigStrings", 
		mapNamedThunkTuples canonicalizeSetString bigStrings, 
		mapNamedTuples canonicalizeSetString bigStringsSoln), 

     IntListTest("extraMembers", extraMembers, extraMembersSoln), 
     IntListTest("extraLists", 
		 mapNamedThunkTuples sortIntList extraLists, 
		 mapNamedTuples sortIntList extraListsSoln),
     BoolTest("extraIsEmpties", extraIsEmpties, extraIsEmptiesSoln),
     IntTest("extraSizes", extraSizes, extraSizesSoln),
     IntListTest("extraPreds", extraPreds, extraPredsSoln),
     StringTest("extraStrings", 
		mapNamedThunkTuples canonicalizeSetString extraStrings, 
		mapNamedTuples canonicalizeSetString extraStringsSoln)
    ]

fun testString n = if n = 1 then "test" else "tests"

fun testOne (name, toString, actualThunk, expected, numPassed, numFailed) =
    let val _ = print ("\n" ^ name ^ ": ")
	val actual = actualThunk ()
    in
	if actual = expected then
	    (print ("passed!"); 
	     numPassed := (! numPassed) + 1)
	else
	    (print ("***FAILED!"); 
	     print ("\n    actual: " ^ (toString actual));
 	     print ("\n  expected: " ^ (toString expected));
	     numFailed := (! numFailed) + 1)
    end

fun helper(groupName, groupToString, groupActual, groupExpected) = 
    let val numPassed = ref 0
	val numFailed = ref 0
    in
	(print ("\n---------------------------------------"); 
	 print ("\nTesting group " ^ groupName ^ ": "); 
	 List.app (fn ((name, actualThunk), (_, expected)) => 
		      testOne(name, groupToString, actualThunk, expected, numPassed, numFailed))
		  (ListPair.zip(groupActual, groupExpected)); 
	 let 
	     val numTests = (! numPassed) + (! numFailed)
	     val _ = if (! numFailed) = 0 then
			 print (if numTests = 1 then
				    "\nPassed the only test"
				else
				    "\nPassed all " ^ (Int.toString numTests) ^ " tests")
		     else
			 print ("\n\n***FAILED " ^ (Int.toString (! numFailed)) 
				^ " " ^ (testString (! numFailed))
				^ "; passed " ^ (Int.toString (! numPassed)) 
				^ " " ^ (testString (! numPassed)) ^ "\n")
	 in (! numPassed, ! numFailed)
	 end)
    end

fun testOneGroup (IntListTest(name,pairs1,pairs2)) = helper(name, list int, pairs1, pairs2)
  | testOneGroup (BoolTest(name,pairs1,pairs2)) = helper(name, bool, pairs1, pairs2)
  | testOneGroup (IntTest(name,pairs1,pairs2)) = helper(name, int, pairs1, pairs2)
  | testOneGroup (StringTest(name,pairs1,pairs2)) = helper(name, quotedString, pairs1, pairs2)

fun testAllCases(testCases) = 
    let val numPassed = ref 0
	val numFailed = ref 0
	fun testOneCase testCase = 
	    let val (passes, fails) = testOneGroup testCase
		val _ = numPassed := (! numPassed) + passes
		val _ = numFailed := (! numFailed) + fails
	    in ()
	    end
            handle exn => print ("***ERROR**: " ^ (exnName exn) ^ " " ^ (exnMessage exn))

	val _ = List.app testOneCase testCases
    in (print ("\n---------------------------------------"); 
       if (! numFailed) = 0 then
	   (print ("\nPassed all " ^ (Int.toString (! numPassed)) ^ " tests\n\n"); 
	    (true, (! numPassed), (! numFailed)))
       else
	   (print ("\n***FAILED " ^ (Int.toString (! numFailed)) ^ " tests; passed " ^ (Int.toString (! numPassed)) ^ " tests\n\n"); 	 
	    (false, ! numPassed, ! numFailed)))
    end

fun testAll() = testAllCases(testCases)
                handle exn => (print ("***ERROR**: " ^ (exnName exn) ^ " " ^ (exnMessage exn)); (false,~1,~1))

val _ = Control.Print.printLength := 1000; 
val _ = Control.Print.stringDepth := 1000; 
val _ = testAll() (* force printing of test cases by side effect *)

end (* struct *)
