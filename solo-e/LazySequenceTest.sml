use "LazySequence.sml";
use "../utils/Show.sml";

open Show; (* toString functions *)
open LazySequence;

signature LAZY_SEQUENCE_TEST = sig
  (* Having an empty signature avoids display any module elements;
     only test results from print are shown. *)
  (* val reallyBigSeq: int LazySequence.t
     val reallyBigSeq2: int LazySequence.t *)
end

structure LazySequenceTest :> LAZY_SEQUENCE_TEST = 
struct

(********************************************************************************
 Setup 
 ********************************************************************************)

val _ = Control.Print.printLength := 1000

val numPassed = ref 0
val numFailed = ref 0

(********************************************************************************
 Helper functions
 ********************************************************************************)

fun checkResult (name, resultToString, expected, actualThunk) = 
    let val _ = print ("\n" ^ name ^ ": ")
	val actual = actualThunk()
    in if actual = expected then 
	   (numPassed := 1 + !numPassed; 
	    print "passed")
       else 
	   (numFailed := 1 + !numFailed; 
	    print "***FAILED";
	    print "\n expected: "; 
	    print (resultToString expected);
	    print "\n   actual: "; 
	    print (resultToString actual))
    end
    handle (IndexOutOfBounds n) => 
	   (numFailed := 1 + !numFailed; 
	    print ("***ERROR: IndexOutOfBounds -- " ^ (Int.toString n)))
	 | exn => (numFailed := 1 + !numFailed; 
		   print ("***ERROR: " ^ (exnName exn) ^ " -- " ^ (exnMessage exn)))

fun testLength (name, seq, expected) = 
    checkResult ("testLength " ^ name, int, expected, fn () => length seq)

fun testToList (name, seq, eltToString, expected) = 
    checkResult ("testToList " ^ name, list eltToString, expected, fn () =>  toList seq)

fun testGet (name, seq, eltToString, expected) = 
    let fun actualThunk () = List.map (fn index => (index, get index seq)) (Utils.range 0 (length seq))
    in checkResult ("testGet " ^ name, list(pair(int,eltToString)), expected, actualThunk)
    end
	   
fun testGetRange (name, seq, lo, hi, eltToString, expected) =
    let fun actualThunk () = List.map (fn index => (index, get index seq)) (Utils.range lo hi)
    in checkResult ("testGetRange " ^ name, list(pair(int,eltToString)), expected, actualThunk)
    end

fun testGetRangeHandleException (name, seq, lo, hi, eltToString, expected) =
  let fun getHandleException (index) =
	eltToString(get index seq)
        handle  (IndexOutOfBounds n) => "Error: IndexOutOfBounds -- " ^ (Int.toString n)
	     |   exn => "Error: " ^ (exnName exn) ^ " -- " ^ (exnMessage exn)
      fun actualThunk () = List.map (fn index => (index, getHandleException(index)))
				    (Utils.range lo hi)
  in checkResult ("testGetRangeHandleException " ^ name, list(pair(int,quotedString)),
		  expected, actualThunk)
  end

fun testSequenceCreationThunk thunk = 
    let val seqVal = thunk() 
    in (numPassed := 1 + !numPassed; 
	print ("\nSequence creation passed (succesfully created a sequence, not necessarily the correct one)");
	seqVal)
    end
    handle
    exn => (numFailed := 1 + !numFailed;
            print ("\n***ERROR when creating sequence: " ^ (exnName exn) ^ " -- " ^ (exnMessage exn));
	    print ("\n***FAILED to create sequence"); 
	    print ("\nBecause an error occurred in sequence creation, returned a 0-length ThunkList instead, which will cause other tests on this segment to fail.");
	    fromList [])

fun printStars() = print "\n************************************************************"
fun printDashes() = print "\n------------------------------------------------------------"

val _ = printStars()

val _ = print "\nTesting LazySequences"

(********************************************************************************
 fromList1 tests
 ********************************************************************************)
val _ = printDashes();
val _  = print "\nval fromList1 = fromList [3, 5, 2, 4]"; 
val fromList1 = testSequenceCreationThunk (fn () => fromList [3, 5, 2, 4])
val _ = (
testLength("fromList1", fromList1, 4); 
testGet("fromList1", fromList1, int, [(0,3), (1, 5), (2,2), (3,4)]); 
testToList("testToList fromList1", fromList1, int, [3, 5, 2, 4])
)

val _ = printDashes()
val _ = print "\nval mapFromList1 = map (fn n => 20 div n) fromList1";
val mapFromList1 = testSequenceCreationThunk (fn () => map (fn n => 20 div n) fromList1)
val _ = (
testLength("mapFromList1", mapFromList1, 4); 
testGet("mapFromList1", 
	mapFromList1, 
	int, 
	[(0,6),(1,4),(2,10),(3,5)]);
testGetRange("mapFromList1", 
	     mapFromList1, 1, 3, 
	     int, 
	     [(1,4),(2,10)]);
testToList("testToList mapFromList1", mapFromList1, int, [6, 4, 10, 5])
)

(********************************************************************************
 fromList2 tests
 ********************************************************************************)

val _ = printDashes()
val _ = print "\nval fromList2 = fromList [3, 0, 5, 2, 4]"
val _ = print "\nval mapFromList2 = map (fn n => 20 div n) fromList2"
val fromList2 = testSequenceCreationThunk (fn () => fromList [3, 0, 5, 2, 4])
val mapFromList2 = testSequenceCreationThunk (fn () => map (fn n => 20 div n) fromList2)
val _ = (
testLength("mapFromList2", mapFromList2, 5);
testGetRange("mapFromList2", 
	     mapFromList2, 2, 5, 
	     int, 
	     [(2,4),(3,10),(4,5)]);
testGetRangeHandleException("mapFromList2", 
			    mapFromList2, 0, 5,
			    int, 
			    [(0,"6"), (1, "Error: Div -- divide by zero"), (2, "4"), (3, "10"), (4, "5")])
)

(********************************************************************************
 segment1 tests
 ********************************************************************************)
val _ = printDashes();
val _ = print "\nval segment1 = segment ~3 4 (fn n => n*n + 6*n)"; 
val segment1 = testSequenceCreationThunk (fn () => segment ~3 4 (fn n => n*n + 6*n))
val _ = (
testLength("segment1", segment1, 7); 
testGet("segment1", segment1, int, [(0,~9),(1,~8),(2,~5),(3,0),(4,7),(5,16),(6,27)]);
testToList("testToList segment1", segment1, int, [~9,~8,~5,0,7,16,27]);
testGetRangeHandleException("segment1", 
			    segment1, ~2, 9, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~9"),(1,"~8"),(2,"~5"),(3,"0"),(4,"7"),(5,"16"),(6,"27"),
			     (7,"Error: IndexOutOfBounds -- 7"),(8,"Error: IndexOutOfBounds -- 8")])
)



val _ = printDashes();
val _ = print "\nval map_dbl_segment1 = map (fn n => n*2) segment1"; 
val map_dbl_segment1 = testSequenceCreationThunk (fn () => map (fn n => n*2) segment1)
val _ = (
testLength("map_dbl_segment1", map_dbl_segment1, 7); 
testGet("map_dbl_segment1", map_dbl_segment1, int, 
	[(0,~18),(1,~16),(2,~10),(3,0),(4,14),(5,32),(6,54)]);
testToList("testToList map_dbl_segment1", map_dbl_segment1, int, 
	   [~18,~16,~10,0,14,32,54]);
testGetRangeHandleException("map_dbl_segment1", 
			    map_dbl_segment1, ~2, 9, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~18"),(1,"~16"),(2,"~10"),(3,"0"),(4,"14"),(5,"32"),(6,"54"),
			     (7,"Error: IndexOutOfBounds -- 7"),(8,"Error: IndexOutOfBounds -- 8")])
)

val _ = printDashes()
val _ = print "\nval map_even_segment1 = map (fn n => n mod 2 = 0) segment1";
val map_even_segment1 = testSequenceCreationThunk (fn () => map (fn n => n mod 2 = 0) segment1)
val _ = (
testLength("map_even_segment1", map_even_segment1, 7); 
testGet("map_even_segment1", map_even_segment1, bool, 
	[(0,false),(1,true),(2,false),(3,true),(4,false),(5,true),(6,false)]); 
testToList("testToList map_even_segment1", map_even_segment1, bool, 
	   [false,true,false,true,false,true,false]);
testGetRangeHandleException("map_even_segment1", 
			    map_even_segment1, ~2, 9, bool, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"false"),(1,"true"),(2,"false"),(3,"true"),(4,"false"),(5,"true"),
			     (6,"false"),(7,"Error: IndexOutOfBounds -- 7"),
			     (8,"Error: IndexOutOfBounds -- 8")])
)

val _ = printDashes()
val _ = print "\nval map_100divNminus7_segment1 = map (fn n => 100 div (n - 7)) segment1"
val map_100divNminus7_segment1 = testSequenceCreationThunk (fn () => map (fn n => 100 div (n - 7)) segment1)
val _ = (
testLength("map_100divNminus7_segment1", map_100divNminus7_segment1, 7);
testGetRangeHandleException("map_100divNminus7_segment1", 
			    map_100divNminus7_segment1, ~2, 9, int, 
                            [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~7"),(1,"~7"),(2,"~9"),(3,"~15"),(4,"Error: Div -- divide by zero"),
			     (5,"11"),(6,"5"),(7,"Error: IndexOutOfBounds -- 7"),
			     (8,"Error: IndexOutOfBounds -- 8")])
)

(********************************************************************************
 fromList3 tests
 ********************************************************************************)

val _ = printDashes();
val _ = print "\nval fromList3 = fromList [2,3,5,7,11,17]"
val fromList3 = testSequenceCreationThunk (fn () => fromList [2,3,5,7,11,17])
val _ = (
testLength("fromList3", fromList3, 6); 
testGet("fromList3", fromList3, int, [(0,2),(1,3),(2,5),(3,7),(4,11),(5,17)]);
testToList("testToList fromList3", fromList3, int, [2,3,5,7,11,17]);
testGetRangeHandleException("fromList3", 
			    fromList3, ~2, 9, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"2"),(1,"3"),(2,"5"),(3,"7"),(4,"11"),(5,"17"),
			     (6,"Error: IndexOutOfBounds -- 6"),(7,"Error: IndexOutOfBounds -- 7"),
			     (8,"Error: IndexOutOfBounds -- 8")])
)

val _ = printDashes();
val _ = print "\nval map_dbl_fromList3 = map (fn n => n*2) fromList3"; 
val map_dbl_fromList3 = testSequenceCreationThunk (fn () => map (fn n => n*2) fromList3)
val _ = (
testLength("map_dbl_fromList3", map_dbl_fromList3, 6); 
testGet("map_dbl_fromList3", map_dbl_fromList3, int, 
	[(0,4),(1,6),(2,10),(3,14),(4,22),(5,34)]);
testToList("testToList map_dbl_fromList3", map_dbl_fromList3, int, 
	   [4,6,10,14,22,34]);
testGetRangeHandleException("map_dbl_fromList3", 
			    map_dbl_fromList3, ~2, 9, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"4"),(1,"6"),(2,"10"),(3,"14"),(4,"22"),(5,"34"),
			     (6,"Error: IndexOutOfBounds -- 6"),(7,"Error: IndexOutOfBounds -- 7"),
			     (8,"Error: IndexOutOfBounds -- 8")])
)

val _ = printDashes()
val _ = print "\nval map_even_fromList3 = map (fn n => n mod 2 = 0) fromList3";
val map_even_fromList3 = testSequenceCreationThunk (fn () => map (fn n => n mod 2 = 0) fromList3)
val _ = (
testLength("map_even_fromList3", map_even_fromList3, 6); 
testGet("map_even_fromList3", map_even_fromList3, bool, 
	[(0,true),(1,false),(2,false),(3,false),(4,false),(5,false)]);
testToList("testToList map_even_fromList3", map_even_fromList3, bool, 
	   [true,false,false,false,false,false]);
testGetRangeHandleException("map_even_fromList3", 
			    map_even_fromList3, ~2, 9, bool, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"true"),(1,"false"),(2,"false"),(3,"false"),(4,"false"),(5,"false"),
			     (6,"Error: IndexOutOfBounds -- 6"),(7,"Error: IndexOutOfBounds -- 7"),
			     (8,"Error: IndexOutOfBounds -- 8")])
)

val _ = printDashes()
val _ = print "\nval map_100divNminus7_fromList3 = map (fn n => 100 div (n - 7)) fromList3"
val map_100divNminus7_fromList3 = testSequenceCreationThunk (fn () => map (fn n => 100 div (n - 7)) fromList3)
val _ = (
testLength("map_100divNminus7_fromList3", map_100divNminus7_fromList3, 6);
testGetRangeHandleException("map_100divNminus7_fromList3", 
			    map_100divNminus7_fromList3, ~2, 9, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~20"),(1,"~25"),(2,"~50"),(3,"Error: Div -- divide by zero"),(4,"25"),(5,"10"),
			     (6,"Error: IndexOutOfBounds -- 6"),(7,"Error: IndexOutOfBounds -- 7"),
			     (8,"Error: IndexOutOfBounds -- 8")])
)

(********************************************************************************
 concat1 tests
 ********************************************************************************)

val _ = printDashes()
val _ = print "\nval concat1 = concat (concat segment1 fromList3) (concat map_dbl_segment1 map_dbl_fromList3)"
val concat1 = testSequenceCreationThunk (fn () => 
                  concat (concat segment1 fromList3)
		         (concat map_dbl_segment1 map_dbl_fromList3))
val _ = (
testLength("concat1", concat1, 26); 
testGet("concat1", concat1, int, 
	[(0,~9),(1,~8),(2,~5),(3,0),(4,7),(5,16),(6,27),(7,2),(8,3),(9,5),
	 (10,7),(11,11),(12,17),(13,~18),(14,~16),(15,~10),(16,0),(17,14),
	 (18,32),(19,54),(20,4),(21,6),(22,10),(23,14),(24,22),(25,34)]);
testToList("testToList concat1", concat1, int, 
	   [~9,~8,~5,0,7,16,27,2,3,5,7,11,17,~18,~16,~10,0,14,32,54,4,6,10,14,22,34]);
testGetRangeHandleException("concat1", 
			    concat1, ~2, 28, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~9"),(1,"~8"),(2,"~5"),(3,"0"),(4,"7"),(5,"16"),(6,"27"),(7,"2"),
			     (8,"3"),(9,"5"),(10,"7"),(11,"11"),(12,"17"),(13,"~18"),(14,"~16"),
			     (15,"~10"),(16,"0"),(17,"14"),(18,"32"),(19,"54"),(20,"4"),(21,"6"),
			     (22,"10"),(23,"14"),(24,"22"),(25,"34"),
			     (26,"Error: IndexOutOfBounds -- 26"),(27,"Error: IndexOutOfBounds -- 27")])
)

val _ = printDashes();
val _ = print "\nval map_inc_concat1 = map (fn x => x+1) concat1"; 
val map_inc_concat1 = testSequenceCreationThunk (fn () => map (fn x => x+1) concat1)
val _ = (
testLength("map_inc_concat1", map_inc_concat1, 26); 
testGet("map_inc_concat1", map_inc_concat1, int, 
	[(0,~8),(1,~7),(2,~4),(3,1),(4,8),(5,17),(6,28),(7,3),(8,4),(9,6),
	 (10,8),(11,12),(12,18),(13,~17),(14,~15),(15,~9),(16,1),(17,15),
	 (18,33),(19,55),(20,5),(21,7),(22,11),(23,15),(24,23),(25,35)]);
testToList("testToList map_inc_concat1", map_inc_concat1, int, 
	   [~8,~7,~4,1,8,17,28,3,4,6,8,12,18,~17,~15,~9,1,15,33,55,5,7,11,15,23,35]);
testGetRangeHandleException("map_inc_concat1", 
			    map_inc_concat1, ~2, 28, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~8"),(1,"~7"),(2,"~4"),(3,"1"),(4,"8"),(5,"17"),(6,"28"),(7,"3"),
			     (8,"4"),(9,"6"),(10,"8"),(11,"12"),(12,"18"),(13,"~17"),(14,"~15"),
			     (15,"~9"),(16,"1"),(17,"15"),(18,"33"),(19,"55"),(20,"5"),(21,"7"),
			     (22,"11"),(23,"15"),(24,"23"),(25,"35"),
			     (26,"Error: IndexOutOfBounds -- 26"),(27,"Error: IndexOutOfBounds -- 27")])
)

val _ = printDashes()
val _ = print "\nval map_even_concat1 = map (fn n => n mod 2 = 0) concat1";
val map_even_concat1 = testSequenceCreationThunk (fn () => map (fn n => n mod 2 = 0) concat1)
val _ = (
testLength("map_even_concat1", map_even_concat1, 26); 
testGet("map_even_concat1", map_even_concat1, bool, 
	[(0,false),(1,true),(2,false),(3,true),(4,false),(5,true),(6,false),(7,true),
	 (8,false),(9,false),(10,false),(11,false),(12,false),(13,true),(14,true),
	 (15,true),(16,true),(17,true),(18,true),(19,true),(20,true),(21,true),
	 (22,true),(23,true),(24,true),(25,true)]);
testToList("testToList map_even_concat1", map_even_concat1, bool, 
	   [false,true,false,true,false,true,false,true,false,false,false,false,
	    false,true,true,true,true,true,true,true,true,true,true,true,true,true]);
testGetRangeHandleException("map_even_concat1", 
			    map_even_concat1, ~2, 28, bool, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"false"),(1,"true"),(2,"false"),(3,"true"),(4,"false"),(5,"true"),
			     (6,"false"),(7,"true"),(8,"false"),(9,"false"),(10,"false"),(11,"false"),
			     (12,"false"),(13,"true"),(14,"true"),(15,"true"),(16,"true"),(17,"true"),
			     (18,"true"),(19,"true"),(20,"true"),(21,"true"),(22,"true"),(23,"true"),
			     (24,"true"),(25,"true"),
			     (26,"Error: IndexOutOfBounds -- 26"),(27,"Error: IndexOutOfBounds -- 27")])
)

val _ = printDashes()
val _ = print "\nval map_100divNminus7_concat1 = map (fn n => 100 div (n - 7)) concat1"
val map_100divNminus7_concat1 = testSequenceCreationThunk (fn () => 
                                  map (fn n => 100 div (n - 7)) concat1)
val _ = (
testLength("map_100divNminus7_concat1", map_100divNminus7_concat1, 26);
testGetRangeHandleException("map_100divNminus7_concat1", 
			    map_100divNminus7_concat1, ~2, 28, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~7"),(1,"~7"),(2,"~9"),(3,"~15"),
			     (4,"Error: Div -- divide by zero"),
			     (5,"11"),(6,"5"),(7,"~20"),(8,"~25"),(9,"~50"),
			     (10,"Error: Div -- divide by zero"),
			     (11,"25"),(12,"10"),(13,"~4"),(14,"~5"),(15,"~6"),(16,"~15"),(17,"14"),
			     (18,"4"),(19,"2"),(20,"~34"),(21,"~100"),(22,"33"),(23,"14"),(24,"6"),(25,"3"),
			     (26,"Error: IndexOutOfBounds -- 26"),(27,"Error: IndexOutOfBounds -- 27")])
)

val _ = printDashes()
val _ = print "\n(* subscriptError3Or4Mod5 raises a subscript error for any number that's 3 or 4 mod 5; otherwise it returns the number *)"
val _ = print "\nfun subscriptError3Or4Mod5 n = (List.nth(Utils.range 0 3, n mod 5); n)"
(* subscriptError3Or4Mod5 raises a subscript error for any number that's 3 or 4 mod 5; otherwise it returns the number *)
fun subscriptError3Or4Mod5 n = (List.nth(Utils.range 0 3, n mod 5); n)
val _ = print "\nval map_subscriptError4Mod5_map_inc_concat1 = map subscriptError4Mod5 map_inc_concat1"
val map_subscriptError3Or4Mod5_map_inc_concat1 = 
    testSequenceCreationThunk (fn () => map subscriptError3Or4Mod5 map_inc_concat1)
val _ = (
testLength("map_subscriptError3Or4Mod5_map_inc_concat1", 
	   map_subscriptError3Or4Mod5_map_inc_concat1, 26); 
testGetRangeHandleException("map_subscriptError3Or4Mod5_map_inc_concat1", 
			    map_subscriptError3Or4Mod5_map_inc_concat1, ~2, 28, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~8"),
			     (1,"Error: Subscript -- subscript out of bounds"),
			     (2,"~4"),(3,"1"),
			     (4,"Error: Subscript -- subscript out of bounds"),
			     (5,"17"),
			     (6,"Error: Subscript -- subscript out of bounds"),
			     (7,"Error: Subscript -- subscript out of bounds"),
			     (8,"Error: Subscript -- subscript out of bounds"),
			     (9,"6"),
			     (10,"Error: Subscript -- subscript out of bounds"),
			     (11,"12"),
			     (12,"Error: Subscript -- subscript out of bounds"),
			     (13,"Error: Subscript -- subscript out of bounds"),
			     (14,"~15"),(15,"~9"),(16,"1"),(17,"15"),
			     (18,"Error: Subscript -- subscript out of bounds"),
			     (19,"55"),(20,"5"),(21,"7"),(22,"11"),(23,"15"),
			     (24,"Error: Subscript -- subscript out of bounds"),
			     (25,"35"),
			     (26,"Error: IndexOutOfBounds -- 26"),
			     (27,"Error: IndexOutOfBounds -- 27")])
)

(********************************************************************************
 concat2 tests
 ********************************************************************************)

val _ = printDashes()
val _ = print "\nval concat2 = concat (concat segment1 (concat map_dbl_segment1"
val _ = print "\n                                              map_100divNminus7_segment1))"
val _ = print "\n                     (concat (concat fromList3 map_dbl_fromList3)"
val _ = print "\n                             map_100divNminus7_fromList3)"
val concat2 = 
  testSequenceCreationThunk (fn () => 
    concat (concat segment1 (concat map_dbl_segment1 
                                    map_100divNminus7_segment1))
           (concat (concat fromList3 map_dbl_fromList3) 
		   map_100divNminus7_fromList3))
val _ = (
testLength("concat2", concat2, 39); 
testGetRangeHandleException("concat2", 
			    concat2, ~2, 41, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~9"),(1,"~8"),(2,"~5"),(3,"0"),(4,"7"),(5,"16"),(6,"27"),
			     (7,"~18"),(8,"~16"),(9,"~10"),(10,"0"),(11,"14"),(12,"32"),
			     (13,"54"),(14,"~7"),(15,"~7"),(16,"~9"),(17,"~15"),
			     (18,"Error: Div -- divide by zero"),
			     (19,"11"),(20,"5"),(21,"2"),(22,"3"),(23,"5"),(24,"7"),(25,"11"),
			     (26,"17"),(27,"4"),(28,"6"),(29,"10"),(30,"14"),(31,"22"),
			     (32,"34"),(33,"~20"),(34,"~25"),(35,"~50"),
			     (36,"Error: Div -- divide by zero"),
			     (37,"25"),(38,"10"),
			     (39,"Error: IndexOutOfBounds -- 39"),(40,"Error: IndexOutOfBounds -- 40")])
)

val _ = printDashes();
val _ = print "\nval map_inc_concat2 = map (fn x => x+1) concat2"; 
val map_inc_concat2 = testSequenceCreationThunk (fn () => map (fn x => x+1) concat2)
val _ = (
testLength("map_inc_concat2", map_inc_concat2, 39); 
testGetRangeHandleException("map_inc_concat2", 
			    map_inc_concat2, ~2, 41, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~8"),(1,"~7"),(2,"~4"),(3,"1"),(4,"8"),(5,"17"),(6,"28"),
			     (7,"~17"),(8,"~15"),(9,"~9"),(10,"1"),(11,"15"),(12,"33"),
			     (13,"55"),(14,"~6"),(15,"~6"),(16,"~8"),(17,"~14"),
			     (18,"Error: Div -- divide by zero"),
			     (19,"12"),(20,"6"),(21,"3"),(22,"4"),(23,"6"),(24,"8"),(25,"12"),
			     (26,"18"),(27,"5"),(28,"7"),(29,"11"),(30,"15"),(31,"23"),
			     (32,"35"),(33,"~19"),(34,"~24"),(35,"~49"),
			     (36,"Error: Div -- divide by zero"),
			     (37,"26"),(38,"11"),
			     (39,"Error: IndexOutOfBounds -- 39"),(40,"Error: IndexOutOfBounds -- 40")])
)

val _ = printDashes()
val _ = print "\nval map_even_concat2 = map (fn n => n mod 2 = 0) concat2";
val map_even_concat2 = testSequenceCreationThunk (fn () => map (fn n => n mod 2 = 0) concat2)
val _ = (
testLength("map_even_concat2", map_even_concat2, 39); 
testGetRangeHandleException("map_even_concat2", 
			    map_even_concat2, ~2, 41, bool, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"false"),(1,"true"),(2,"false"),(3,"true"),(4,"false"),
			     (5,"true"),(6,"false"),(7,"true"),(8,"true"),(9,"true"),
			     (10,"true"),(11,"true"),(12,"true"),(13,"true"),(14,"false"),
			     (15,"false"),(16,"false"),(17,"false"),
			     (18,"Error: Div -- divide by zero"),
			     (19,"false"),(20,"false"),(21,"true"),(22,"false"),(23,"false"),
			     (24,"false"),(25,"false"),(26,"false"),(27,"true"),(28,"true"),
			     (29,"true"),(30,"true"),(31,"true"),(32,"true"),(33,"true"),
			     (34,"false"),(35,"true"),
			     (36,"Error: Div -- divide by zero"),
			     (37,"false"),(38,"true"),(39,"Error: IndexOutOfBounds -- 39"),
			     (40,"Error: IndexOutOfBounds -- 40")])
)

val _ = printDashes()
val _ = print "\nval map_subscriptError3Or4Mod5_map_inc_concat2 = map subscriptError3Or4Mod5 map_inc_concat2"
val map_subscriptError3Or4Mod5_map_inc_concat2 = 
  testSequenceCreationThunk (fn () => map subscriptError3Or4Mod5 map_inc_concat2)
val _ = (
testLength("map_subscriptError3Or4Mod5_map_inc_concat2", 
	   map_subscriptError3Or4Mod5_map_inc_concat2, 39); 
testGetRangeHandleException("map_subscriptError3Or4Mod5_map_inc_concat2", 
			    map_subscriptError3Or4Mod5_map_inc_concat2, ~2, 41, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"~8"),(1,"Error: Subscript -- subscript out of bounds"),
			     (2,"~4"),(3,"1"),
			     (4,"Error: Subscript -- subscript out of bounds"),
			     (5,"17"),
			     (6,"Error: Subscript -- subscript out of bounds"),
			     (7,"Error: Subscript -- subscript out of bounds"),
			     (8,"~15"),(9,"~9"),(10,"1"),(11,"15"),
			     (12,"Error: Subscript -- subscript out of bounds"),
			     (13,"55"),
			     (14,"Error: Subscript -- subscript out of bounds"),
			     (15,"Error: Subscript -- subscript out of bounds"),
			     (16,"~8"),(17,"~14"),
			     (18,"Error: Div -- divide by zero"),
			     (19,"12"),(20,"6"),
			     (21,"Error: Subscript -- subscript out of bounds"),
			     (22,"Error: Subscript -- subscript out of bounds"),
			     (23,"6"),
			     (24,"Error: Subscript -- subscript out of bounds"),
			     (25,"12"),
			     (26,"Error: Subscript -- subscript out of bounds"),
			     (27,"5"),(28,"7"),(29,"11"),(30,"15"),
			     (31,"Error: Subscript -- subscript out of bounds"),
			     (32,"35"),(33,"~19"),(34,"~24"),(35,"~49"),
			     (36,"Error: Div -- divide by zero"),
			     (37,"26"),(38,"11"),
			     (39,"Error: IndexOutOfBounds -- 39"),
			     (40,"Error: IndexOutOfBounds -- 40")])
)

(********************************************************************************
 emptySegment tests
 ********************************************************************************)

val _ = printDashes()
val _ = print "\nval emptySegment = segment 5 5 (fn x => x)"
val emptySegment = testSequenceCreationThunk (fn () => segment 5 5 (fn x => x))
val _ = (
testLength("emptySegment", emptySegment, 0); 
testGet("emptySegment", emptySegment, int, []);
testToList("testToList emptySegment", emptySegment, int, []);
testGetRangeHandleException("emptySegment", 
			    emptySegment, ~2, 2, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),
			     (~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"Error: IndexOutOfBounds -- 0"),
			     (1,"Error: IndexOutOfBounds -- 1")])
)

val _ = printDashes()
val _ = print "\nval map_dbl_emptySegment = map (fn n => n*2) emptySegment"
val map_dbl_emptySegment = testSequenceCreationThunk (fn () => map (fn n => n*2) emptySegment)
val _ = (
testLength("map_dbl_emptySegment", map_dbl_emptySegment, 0); 
testGet("map_dbl_emptySegment", map_dbl_emptySegment, int, []);
testToList("testToList map_dbl_emptySegment", map_dbl_emptySegment, int, []);
testGetRangeHandleException("map_dbl_emptySegment", 
			    map_dbl_emptySegment, ~2,2, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),
			     (~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"Error: IndexOutOfBounds -- 0"),
			     (1,"Error: IndexOutOfBounds -- 1")])
)

val _ = printDashes()
val _ = print "\nval map_even_emptySegment = map (fn n => n mod 2 = 0) emptySegment";
val map_even_emptySegment = 
    testSequenceCreationThunk (fn () => map (fn n => n mod 2 = 0) emptySegment)
val _ = (
testLength("map_even_emptySegment", map_even_emptySegment, 0); 
testGet("map_even_emptySegment", map_even_emptySegment, bool, []);
testToList("testToList map_even_emptySegment", map_even_emptySegment, bool, []);
testGetRangeHandleException("map_even_emptySegment", 
			    map_even_emptySegment, ~2, 2, bool, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),
			     (~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"Error: IndexOutOfBounds -- 0"),
			     (1,"Error: IndexOutOfBounds -- 1")])
)

val _ = printDashes()
val _ = print "\nval map_100divNminus7_emptySegment = map (fn n => 100 div (n - 7)) emptySegment"
val map_100divNminus7_emptySegment = 
  testSequenceCreationThunk (fn () => map (fn n => 100 div (n - 7)) emptySegment)
val _ = (
testLength("map_100divNminus7_emptySegment", map_100divNminus7_emptySegment, 0);
testGetRangeHandleException("map_100divNminus7_emptySegment", 
			    map_100divNminus7_emptySegment, ~2, 2, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),
			     (~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"Error: IndexOutOfBounds -- 0"),
			     (1,"Error: IndexOutOfBounds -- 1")])
)

(********************************************************************************
 concat3 tests
 ********************************************************************************)

val _ = printDashes()
val _ = print "\n(* concat3 is a two-element sequence containing 7 and 42 formed from empty and singleton sequences *)"
val _ = print "\nval concat3 = concat (concat (concat emptySegment (fromList [7]))"
val _ = print "\n                                     emptySegment)"
val _ = print "\n                     (concat (fromList [])"
val _ = print "\n                             (concat (segment 4 5 (fn n => n*10 + 2))"
val _ = print "\n                                     emptySegment))"
(* concat3 is a two-element sequence containing 7 and 20 formed from empty and singleton sequences *)
val concat3 = 
  testSequenceCreationThunk (fn () => 
    concat (concat (concat emptySegment (fromList [7]))
		   emptySegment)
	   (concat (fromList []) 
		   (concat (segment 4 5 (fn n => n*10 + 2))
			   emptySegment)))
val _ = (
testLength("concat3", concat3, 2);
testGet("concat3", concat3, int, [(0,7),(1,42)]);
testToList("testToList concat3", concat3, int, [7,42]);
testGetRangeHandleException("concat3", 
			    concat3, ~2, 4, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"7"),(1,"42"),
			     (2,"Error: IndexOutOfBounds -- 2"),(3,"Error: IndexOutOfBounds -- 3")])
)

val _ = printDashes();
val _ = print "\nval map_inc_concat3 = map (fn x => x+1) concat3"; 
val map_inc_concat3 = testSequenceCreationThunk (fn () => map (fn x => x+1) concat3)
val _ = (
testLength("map_inc_concat3", map_inc_concat3, 2); 
testGet("map_inc_concat3", map_inc_concat3, int, [(0,8),(1,43)]);
testToList("testToList map_inc_concat3", map_inc_concat3, int, [8,43]);
testGetRangeHandleException("map_inc_concat3", 
			    map_inc_concat3, ~2, 4, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"8"),(1,"43"),
			     (2,"Error: IndexOutOfBounds -- 2"),(3,"Error: IndexOutOfBounds -- 3")])
)

val _ = printDashes()
val _ = print "\nval map_even_concat3 = map (fn n => n mod 2 = 0) concat3";
val map_even_concat3 = testSequenceCreationThunk (fn () => map (fn n => n mod 2 = 0) concat3)
val _ = (
testLength("map_even_concat3", map_even_concat3, 2); 
testGet("map_even_concat3", map_even_concat3, bool, [(0,false),(1,true)]);
testToList("testToList map_even_concat3", map_even_concat3, bool, [false, true]);
testGetRangeHandleException("map_even_concat3", 
			    map_even_concat3, ~2, 4, bool, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"false"),(1,"true"),
			     (2,"Error: IndexOutOfBounds -- 2"),(3,"Error: IndexOutOfBounds -- 3")])
)

val _ = printDashes()
val _ = print "\nval map_100divNminus7_concat3 = map (fn n => 100 div (n - 7)) concat3"
val map_100divNminus7_concat3 = 
    testSequenceCreationThunk (fn () => map (fn n => 100 div (n - 7)) concat3)
val _ = (
testLength("map_100divNminus7_concat3", map_100divNminus7_concat3, 2);
testGetRangeHandleException("map_100divNminus7_concat3", 
			    map_100divNminus7_concat3, ~2, 4, int, 
			    [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
			     (0,"Error: Div -- divide by zero"), (1, "2"), 
			     (2,"Error: IndexOutOfBounds -- 2"),(3,"Error: IndexOutOfBounds -- 3")])
)

(********************************************************************************
 linearIncrement and quadraticDouble definitions
 ********************************************************************************)

val _ = printDashes()
val _ = print "\n(* A slow incrementing function *)"
val _ = print "\nfun linearIncrement n ="
val _ = print "\n  let fun loop num ans ="
val _ = print "\n        if num = 0 then ans else loop (num-1) (ans+1)"
val _ = print "\n   in loop n 1"
val _ = print "\n   end"

(* A slow incrementing function *)
fun linearIncrement n =
  let fun loop num ans =
	if num = 0 then ans else loop (num-1) (ans+1)
  in loop n 1
 end

val _ = print "\n\n(* A very slow doubling function *)"
val _ = print "\nfun quadraticDouble n ="
val _ = print "\n  let fun loop num ans ="
val _ = print "\n        if num = 0 then ans else loop (num-1) (linearIncrement ans)"
val _ = print "\n   in loop n n"
val _ = print "\n   end"

(* A very slow doubling function *)
fun quadraticDouble n =
  let fun loop num ans =
	if num = 0 then ans else loop (num-1) (linearIncrement ans)
  in loop n n
 end

(********************************************************************************
 reallyBigSeq
 ********************************************************************************)

val _ = printDashes()
val _ = print "\nval reallyBigSeq segment ~200000000 200000000 quadraticDouble"
val reallyBigSeq = 
  testSequenceCreationThunk (fn () => segment ~200000000 200000000 quadraticDouble)
val _ = (
testLength("reallyBigSeq", reallyBigSeq, 400000000); 
testGetRange("reallyBigSeq", 
	     reallyBigSeq, 
	     200000000,
	     200000010,
	     int,
	     [(200000000,0),(200000001,2),(200000002,4),(200000003,6),(200000004,8),
	      (200000005,10),(200000006,12),(200000007,14),(200000008,16),(200000009,18)])
)

val _ = printDashes();
val _ = print "\nval map_half_reallyBigSeq = map (fn x => x div 2) reallyBigSeq"; 
val map_half_reallyBigSeq = 
  testSequenceCreationThunk (fn () => map (fn x => x div 2) reallyBigSeq)
val _ = (
testLength("map_half_reallyBigSeq", map_half_reallyBigSeq, 400000000);
testGetRange("map_half_reallyBigSeq", 
	     map_half_reallyBigSeq, 
	     200000000,
	     200000010,
	     int,
	     [(200000000,0),(200000001,1),(200000002,2),(200000003,3),(200000004,4),
	      (200000005,5),(200000006,6),(200000007,7),(200000008,8),(200000009,9)])
)

val _ = printDashes()
val _ = print "\nval map_isHalfEven_reallyBigSeq = map (fn n => n mod 2 = 0) reallyBigSeq";
val map_isHalfEven_reallyBigSeq = 
  testSequenceCreationThunk (fn () => map (fn n => (n div 2) mod 2 = 0) reallyBigSeq)
val _ = (
testLength("map_isHalfEven_reallyBigSeq", map_isHalfEven_reallyBigSeq, 400000000);
testGetRange("map_isHalfEven_reallyBigSeq", map_isHalfEven_reallyBigSeq, 
	     200000000,
	     200000010,
	     bool,
	     [(200000000,true),(200000001,false),(200000002,true),(200000003,false),(200000004,true),
	      (200000005,false),(200000006,true),(200000007,false),(200000008,true),(200000009,false)])
)

val _ = printDashes()
val _ = print "\nval map_100divHalfNminus7_reallyBigSeq = map (fn n => 100 div ((n div 2) - 7)) reallyBigSeq"
val map_100divHalfNminus7_reallyBigSeq = 
  testSequenceCreationThunk (fn () => map (fn n => 100 div ((n div 2) - 7)) reallyBigSeq)
val _ = (
testLength("map_100divHalfNminus7_reallyBigSeq", map_100divHalfNminus7_reallyBigSeq, 400000000);
testGetRangeHandleException("map_100divHalfNminus7_reallyBigSeq", 
			    map_100divHalfNminus7_reallyBigSeq, 
			    200000000,
			    200000010,
			    int,
			    [(200000000,"~15"),
			     (200000001,"~17"), 
			     (200000002,"~20"),
			     (200000003,"~25"),
			     (200000004,"~34"), 
			     (200000005,"~50"), 
			     (200000006,"~100"), 
			     (200000007,"Error: Div -- divide by zero"), 
			     (200000008,"100"),
			     (200000009,"50")])
)

val _ = printDashes()
val _ = print "\nval map_subscriptError4Mod5_map_half_reallyBigSeq = map subscriptError4Mod5 map_half_reallyBigSeq"
val map_subscriptError3Or4Mod5_map_half_reallyBigSeq = 
  testSequenceCreationThunk (fn () => map subscriptError3Or4Mod5 map_half_reallyBigSeq)
val _ = (
testLength("map_subscriptError3Or4Mod5_map_half_reallyBigSeq", 
	   map_subscriptError3Or4Mod5_map_half_reallyBigSeq, 400000000);
testGetRangeHandleException("map_subscriptError3Or4Mod5_map_half_reallyBigSeq", 
			    map_subscriptError3Or4Mod5_map_half_reallyBigSeq, 
			    200000000,
			    200000010,
			    int,
			    [(200000000,"0"),
			     (200000001,"1"), 
			     (200000002,"2"),
			     (200000003,"Error: Subscript -- subscript out of bounds"),
			     (200000004,"Error: Subscript -- subscript out of bounds"),
			     (200000005,"5"),
			     (200000006,"6"), 
			     (200000007,"7"),
			     (200000008,"Error: Subscript -- subscript out of bounds"),
			     (200000009,"Error: Subscript -- subscript out of bounds")])
)

(********************************************************************************
 reallyBigSeq2
 ********************************************************************************)

val reallyBigSeq2 = 
  testSequenceCreationThunk (fn () => concat reallyBigSeq (map quadraticDouble reallyBigSeq))
val _ = (
testLength("reallyBigSeq2", reallyBigSeq2, 800000000); 
testGetRange("reallyBigSeq2", 
	     reallyBigSeq2, 
	     200000000,
	     200000010,
	     int,
	     [(200000000,0),(200000001,2),(200000002,4),(200000003,6),(200000004,8),
	      (200000005,10),(200000006,12),(200000007,14),(200000008,16),(200000009,18)]);
testGetRange("reallyBigSeq2", 
	     reallyBigSeq2, 
	     600000000,
	     600000010,
	     int,
	     [(600000000,0),(600000001,4),(600000002,8),(600000003,12),(600000004,16),
	      (600000005,20),(600000006,24),(600000007,28),(600000008,32),(600000009,36)])
)


val _ = printDashes();
val _ = print "\nval map_quarter_reallyBigSeq2 = map (fn x => x div 4) reallyBigSeq2"; 
val map_quarter_reallyBigSeq2 = 
  testSequenceCreationThunk (fn () => map (fn x => x div 4) reallyBigSeq2)
val _ = (
testLength("map_quarter_reallyBigSeq2", map_quarter_reallyBigSeq2, 800000000);
testGetRange("map_quarter_reallyBigSeq2", 
	     map_quarter_reallyBigSeq2, 
	     200000000,
	     200000010,
	     int,
	     [(200000000,0),(200000001,0),(200000002,1),(200000003,1),(200000004,2),
	      (200000005,2),(200000006,3),(200000007,3),(200000008,4),(200000009,4)]);
testGetRange("map_quarter_reallyBigSeq2", 
	     map_quarter_reallyBigSeq2, 
	     600000000,
	     600000010,
	     int,
	     [(600000000,0),(600000001,1),(600000002,2),(600000003,3),(600000004,4),
	      (600000005,5),(600000006,6),(600000007,7),(600000008,8),(600000009,9)])
)

val _ = printDashes()
val _ = print "\nval map_isQuarterEven_reallyBigSeq2 = map (fn n => n mod 2 = 0) reallyBigSeq2";
val map_isQuarterEven_reallyBigSeq2 = 
  testSequenceCreationThunk (fn () => map (fn n => (n div 2) mod 2 = 0) reallyBigSeq2)
val _ = (
testLength("map_isQuarterEven_reallyBigSeq2", map_isQuarterEven_reallyBigSeq2, 800000000);
testGetRange("map_isQuarterEven_reallyBigSeq2", map_isQuarterEven_reallyBigSeq2, 
	     200000000,
	     200000010,
	     bool,
	     [(200000000,true),(200000001,false),(200000002,true),(200000003,false),(200000004,true),
	      (200000005,false),(200000006,true),(200000007,false),(200000008,true),(200000009,false)])
)

val _ = printDashes()
val _ = print "\nval map_100divQuarterNminus7_reallyBigSeq2 = map (fn n => 100 div ((n div 4) - 7)) reallyBigSeq2"
val map_100divQuarterNminus7_reallyBigSeq2 = 
  testSequenceCreationThunk (fn () => map (fn n => 100 div ((n div 4) - 7)) reallyBigSeq2)
val _ = (
testLength("map_100divQuarterNminus7_reallyBigSeq2", map_100divQuarterNminus7_reallyBigSeq2, 800000000);
testGetRangeHandleException("map_100divQuarterNminus7_reallyBigSeq2", 
			    map_100divQuarterNminus7_reallyBigSeq2, 
			    200000000,
			    200000010,
			    int,
			    [(200000000,"~15"),
			     (200000001,"~15"),
			     (200000002,"~17"),
			     (200000003,"~17"),
			     (200000004,"~20"),
			     (200000005,"~20"),
			     (200000006,"~25"),
			     (200000007,"~25"),
			     (200000008,"~34"),
			     (200000009,"~34")]);
testGetRangeHandleException("map_100divQuarterNminus7_reallyBigSeq2", 
			    map_100divQuarterNminus7_reallyBigSeq2, 
			    600000000,
			    600000010,
			    int,
			    [(600000000,"~15"),
			     (600000001,"~17"), 
			     (600000002,"~20"),
			     (600000003,"~25"),
			     (600000004,"~34"), 
			     (600000005,"~50"), 
			     (600000006,"~100"), 
			     (600000007,"Error: Div -- divide by zero"), 
			     (600000008,"100"),
			     (600000009,"50")])
)


val _ = printDashes()
val _ = print "\nval map_subscriptError4Mod5_map_quarter_reallyBigSeq2 = map subscriptError4Mod5 map_quarter_reallyBigSeq2"
val map_subscriptError3Or4Mod5_map_quarter_reallyBigSeq2 = 
  testSequenceCreationThunk (fn () => map subscriptError3Or4Mod5 map_quarter_reallyBigSeq2)
val _ = (
testLength("map_subscriptError3Or4Mod5_map_quarter_reallyBigSeq2", 
	   map_subscriptError3Or4Mod5_map_quarter_reallyBigSeq2, 800000000);
testGetRangeHandleException("map_subscriptError3Or4Mod5_map_quarter_reallyBigSeq2", 
			    map_subscriptError3Or4Mod5_map_quarter_reallyBigSeq2, 
			    200000000,
			    200000010,
			    int,
			    [(200000000,"0"),
			     (200000001,"0"), 
			     (200000002,"1"),
			     (200000003,"1"), 
			     (200000004,"2"),
			     (200000005,"2"),
			     (200000006,"Error: Subscript -- subscript out of bounds"),
			     (200000007,"Error: Subscript -- subscript out of bounds"),
			     (200000008,"Error: Subscript -- subscript out of bounds"),
			     (200000009,"Error: Subscript -- subscript out of bounds")]);

testGetRangeHandleException("map_subscriptError3Or4Mod5_map_quarter_reallyBigSeq2", 
			    map_subscriptError3Or4Mod5_map_quarter_reallyBigSeq2, 
			    600000000,
			    600000010,
			    int,
			    [(600000000,"0"),
			     (600000001,"1"),
			     (600000002,"2"),
			     (600000003,"Error: Subscript -- subscript out of bounds"),
			     (600000004,"Error: Subscript -- subscript out of bounds"),
			     (600000005,"5"),
			     (600000006,"6"),
			     (600000007,"7"),
			     (600000008,"Error: Subscript -- subscript out of bounds"),
			     (600000009,"Error: Subscript -- subscript out of bounds")])
)



	
(********************************************************************************
 Test case summary
 ********************************************************************************)

val numTests = (!numPassed) + (!numFailed)

val _ = (
printDashes();
if !numFailed = 0 then
    print ("\nSUMMARY: Passed all " ^ (Int.toString numTests) ^ " test cases")
else
    print ("\nSUMMARY: Failed " ^ (Int.toString (!numFailed)) ^ " of " 
	   ^ (Int.toString numTests) ^ " test cases"); 
printStars();
print "\n"
)

end
