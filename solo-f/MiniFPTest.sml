(* Testing functions for the MINI-FP interpreter *)

use "../solo-f/MiniFPInterp.sml";
use "../utils/Utils.sml";

structure MiniFPTest = struct
  
  open MiniFP
  open MiniFPInterp

  (* Here, the "S" prefix indicates "string" *)

  val vector1S = "(2 3 5)"

  val vector2S = "(10 20 30)"

  val vectorsS = "((2 3 5) (10 20 30))"

  val matrixS = "((1 4) (8 6) (7 9))"
      
  val matrices1S = "(((2 3 5) (10 20 30)) ((1 4) (8 6) (7 9)))"

  val matrices2S = "(((1 4) (8 6) (7 9)) ((2 3 5) (10 20 30)))"

  val ipS = "(o (/ +) (o (a x) trans))"

  val mmS = "(o (a (a (o (/ +) (o (a x) trans)))) (o (a distl) (o distr (1 (o trans 2)))))"

  val testSuite = [

      (* test id *)
      (("id","17"), "17"), 
      (("id", vector1S), vector1S),
      (("id", matrixS), matrixS),
      (("id", matrices1S), matrices1S),

      (* test + *)
      (("+","(7 3)"), "10"), 
      (("+","(7 ~3)"), "4"), 
      (("+","(~7 3)"), "~4"), 
      (("+","(~7 ~3)"), "~10"), 
      (("+","(0 7)"), "7"), 
      (("+","(7 0)"), "7"), 
      (("+","1"), "Error: Ill-formed application: apply + 1"), 
      (("+","(1)"), "Error: Ill-formed application: apply + (1)"), 
      (("+","(1 2 3)"), "Error: Ill-formed application: apply + (1 2 3)"), 

      (* test - *)
      (("-","(7 3)"), "4"), 
      (("-","(7 ~3)"), "10"), 
      (("-","(~7 3)"), "~10"), 
      (("-","(~7 ~3)"), "~4"), 
      (("-","(0 7)"), "~7"), 
      (("-","(7 0)"), "7"), 

      (* test x *)
      (("x","(7 3)"), "21"), 
      (("x","(7 ~3)"), "~21"), 
      (("x","(~7 3)"), "~21"), 
      (("x","(~7 ~3)"), "21"), 
      (("x","(0 7)"), "0"), 
      (("x","(7 0)"), "0"), 

      (* test -: *)
      (("-:","(7 3)"), "2"), 
      (("-:","(7 ~3)"), "~3"), 
      (("-:","(~7 3)"), "~3"), 
      (("-:","(~7 ~3)"), "2"), 
      (("-:","(0 7)"), "0"), 
      (("-:","(7 0)"), "Error: Division by zero"), 

      (* test $ *)
      (("($ 17)","5"), "17"), 
      (("($ 17)", vector1S), "17"),
      (("($ 17)", matrixS), "17"),
      (("($ 17)", matrices1S), "17"),

      (* test selection *)
      (("1", "17"), "Error: Ill-formed application: apply 1 17"),
      (("0", vector1S), "Error: Selection index 0 out of bounds [1..3]"),
      (("1", vector1S), "2"),
      (("2", vector1S), "3"),
      (("3", vector1S), "5"),
      (("4", vector1S), "Error: Selection index 4 out of bounds [1..3]"),
      (("0", vectorsS), "Error: Selection index 0 out of bounds [1..2]"),
      (("1", vectorsS), "(2 3 5)"),
      (("2", vectorsS), "(10 20 30)"),
      (("3", vectorsS), "Error: Selection index 3 out of bounds [1..2]"),

      (* test distl *)
      (("distl", "(7 " ^ vector1S ^ ")"), "((7 2) (7 3) (7 5))"), 
      (("distl", vectorsS), "(((2 3 5) 10) ((2 3 5) 20) ((2 3 5) 30))"), 
      (("distl", "7"), "Error: Ill-formed application: apply distl 7"), 
      (("distl", "(7 4)"),  "Error: Ill-formed application: apply distl (7 4)"), 
      
      (* test distr *)
      (("distr", "(" ^ vector1S ^ " 7)"), "((2 7) (3 7) (5 7))"), 
      (("distr", vectorsS), "((2 (10 20 30)) (3 (10 20 30)) (5 (10 20 30)))"),
      (("distr", "7"), "Error: Ill-formed application: apply distr 7"), 
      (("distr", "(7 4)"), "Error: Ill-formed application: apply distr (7 4)"), 

      (* test trans *)
      (("trans", vectorsS), "((2 10) (3 20) (5 30))"), 
      (("trans", matrixS), "((1 8 7) (4 6 9))"), 
      (("trans",
	"(((2 10) (3 20) (5 30)) ((1 4) (8 6) (7 9)))"), 
        "(((2 10) (1 4)) ((3 20) (8 6)) ((5 30) (7 9)))"), 
      (("trans", vector1S), "Error: transpose -- not a sequence of sequences: (2 3 5)"), 
      (("trans", "17"), "Error: Ill-formed application: apply trans 17"), 
      (("trans", matrices1S), "Error: transpose -- sequences not all of same length: (((2 3 5) (10 20 30)) ((1 4) (8 6) (7 9)))"), 

      (* test bu *)
      (("(bu + 1)", "5"), "6"),
      (("(bu + 10)", "5"), "15"), 
      (("(bu x 2)", "5"), "10"),
      (("(bu x 10)", "5"), "50"),
      (("(bu - 3)", "5"), "~2"),
      (("(bu - 12)", "5"), "7"),
      (("(bu -: 17)", "5"), "3"),
      (("(bu -: 50)", "5"), "10"), 
      (("(bu x 2)", "(3 5)"), "Error: Ill-formed application: apply x (2 (3 5))"), 

      (* test reduction *)
      (("(/ +)", vector1S), "10"),
      (("(/ x)", vector1S), "30"),
      (("(/ -)", vector1S), "4"),
      (("(/ -:)", vector1S), "Error: Division by zero"),
      (("(/ +)", "(1 2 3 4 5 6 7 8 9 10)"), "55"),
      (("(/ x)", "(1 2 3 4 5 6 7 8 9 10)"), "3628800"),
      (("(/ -)", "(10 9 8 7 6 5 4 3 2 1)"), "5"), 
      (("(/ -:)", "(10 9 8 7 6 5 4 3 2 1)"), "10"), 
  
      (* reduction can be done with any binary operator *)
      (("(/ (o + ((o (bu x 7) 1) (o (bu x 100) 2))))", "(1 2 3)"), "31407"),

      (* reduction on singleton sequences *)
      (("(/ +)", "(17)"), "17"), 
      (("(/ x)", "(17)"), "17"), 
      (("(/ -)", "(17)"), "17"), 
      (("(/ -:)", "(17)"), "17"),

      (* reduction on empty sequences *)
      (("(/ +)", "()"), "Error: Reduction of empty sequence"),  
      (("(/ x)", "()"), "Error: Reduction of empty sequence"),  
      (("(/ -)", "()"), "Error: Reduction of empty sequence"),  
      (("(/ -:)", "()"), "Error: Reduction of empty sequence"),  

      (* test mapping *)
      (("(a id)", "17"), "Error: Ill-formed application: apply (a id) 17"), 
      (("(a id)", vector1S), "(2 3 5)"), 
      (("(a ($ 17))", vector1S), "(17 17 17)"), 
      (("(a (bu + 1))", vector1S), "(3 4 6)"), 
      (("(a (bu x 2))", vector1S), "(4 6 10)"), 
      (("(a +)", matrixS), "(5 14 16)"), 
      (("(a (/ +))", vectorsS), "(10 60)"), 
      (("(a (a (/ +)))", matrices1S), "((10 60) (5 14 16))"), 

      (* test composition *)
      (("(o id ($ 17))", vector1S), "17"), 
      (("(o ($ 17) id)", vector1S), "17"), 
      (("(o (a (bu + 1)) (a ($ 17)))", vector1S), "(18 18 18)"), 
      (("(o (a (bu + 1)) (a (bu x 2)))", vector1S), "(5 7 11)"), 
      (("(o (bu + 1) (/ +))", vector1S), "11"), 
      (("(o trans trans)", matrixS), matrixS), 
      (("(a (o (bu -: 50) (o (bu + 1) (bu x 2))))", vector1S), "(10 7 4)"), 
      (("(o (a (bu -: 50)) (o (a (bu + 1)) (a (bu x 2))))", vector1S), "(10 7 4)"), 

      (* function sequences *)
      (("(+ - x -:)", "(17 5)"), "(22 12 85 3)"), 
      (("(distl trans)", vectorsS), "((((2 3 5) 10) ((2 3 5) 20) ((2 3 5) 30)) ((2 10) (3 20) (5 30)))"),
      (("(distr)", vectorsS), "(((2 (10 20 30)) (3 (10 20 30)) (5 (10 20 30))))"), 
      (("((a (/ +)) (a (/ x)) (a (/ -)))", vectorsS), "((10 60) (30 6000) (4 20))"),
      (("((bu -: 50) (bu + 1) (bu x 2))", "10"), "(5 11 20)"), 
      (("((a (bu -: 50)) (a (bu + 1)) (a (bu x 2)))", vector1S), "((25 16 10) (3 4 6) (4 6 10))"), 

      (* big tests *)
      ((ipS, vectorsS), "230"),
      ((ipS, matrixS), "Error: Ill-formed application: apply x (1 8 7)"),

      ((mmS, matrices1S), "((61 71) (380 430))"), 
      ((mmS, matrices2S), "((42 83 125) (76 144 220) (104 201 305))"), 
      ((mmS, vectorsS), "Error: transpose -- not a sequence of sequences: (10 20 30)"),

      (* Tests of F and F_sx from Problem 1a *)
      ((funFormToString F, vector1S), "(20 30 50)"), 
      ((Sexp.sexpToString F_sx, vector1S), "(20 30 50)")	  

  ]

  fun testEntries entries = 
    let val results = List.map testTrial entries
	val mismatches = 
	    List.map (fn ((args,expected), (actual, isEqual)) => (args,expected,actual))
		     (List.filter (fn ((args,expected),
					(actual, isEqual)) => not isEqual)
				  (ListPair.zip(entries, results)))
	val numTrials = List.length entries
	val numFails = List.length mismatches
	val numSuccs = numTrials - numFails
	val _ = print 
	       ("\n============================================================\n"
		^ "TESTING SUMMARY:\n"
		^ "Passed " ^ (Int.toString numSuccs) 
		^ " of " ^ (Int.toString numTrials) 
		^ " test cases.\n")
	in ((if numFails > 0 then 
	       let val _ = print ("The following "
				  ^ (Int.toString numFails)
				  ^ " tests failed:\n\n")
	       in List.app (fn (args,expected,actual) => 
				(printTrialPrefix args;
				 printMismatch expected actual))
			    mismatches
			    
	       end
	    else
	      ()); 
	    (numSuccs, numFails))
    end

  and testEntry (args,expected) =
      let val _ = testTrial (args,expected)
      in ()
      end

  and testTrial (args,expected) =
      let val _ = printTrialPrefix args
	  val actual = trial args
	  val isEqual = resultEqual(expected, actual)
	  val _ = if isEqual then 
		      print ((resultToString actual) ^ " OK!\n")
		  else 
		      printMismatch expected actual
      in (actual, isEqual)
      end

  and printTrialPrefix args =
      print ((callToString args) ^ " = ")

  and printMismatch exp act = 
      print ("***TESTING MISMATCH***"
	     ^ "\n*** Expected: "
	     ^ (resultToString exp)
	     ^ "\n***   Actual: "
	     ^ (resultToString act)
	     ^ "\n\n")

  and trial (funString, objString) =
      objToString (apply (stringToFunForm funString) (stringToObj objString))
      handle (EvalError msg) => "Error: " ^ msg
	   | (SyntaxError msg) => "Error:" ^ msg
	   (* Added by Lyn on 2019/01/04 *)
	   | exn => "Error: " ^ (exnName exn) ^ " " ^ (exnMessage exn)

  and callToString (funString, objString) = "apply " ^ funString ^ " " ^ objString

  and resultEqual (string1, string2) = string1 = string2 (* String equality *)

  and resultToString string = string (* identity *)

  and testApply funString objString = 
      println (objToString (apply (stringToFunForm funString) (stringToObj objString)))

  and println s = (print s; print "\n")

  and testAll () = let val (passes,fails) = testEntries testSuite
		   in if fails = 0 then
			"Passed all " ^ (Int.toString passes) ^ " test cases"
		      else
			"Failed " ^ (Int.toString fails) ^ " of "
			^ (Int.toString(passes+fails))
			^ " test cases; for details, look above."
    end


end;

(* Run the test cases when loading *)			   
MiniFPTest.testAll();
