(* Load PostFix and Condex structures *)
use "Condex.sml"; (* semi-colon required! *)
use "../postfix/PostFix.sml"; (* semi-colon required! *)
use "../utils/Utils.sml"; (* semi-colon required! *)

(* Note that Condex and PostFix structures overlap on the following:
   + types pgm, arithop, relp
   + constructors Int, Add, Sub, Mul, Div, Rem, Lt, Eq,
   For these you need to use explicit qualification. 
   E.g. Condex.Add vs. PostFix.Add
 *)

structure CondexToPostFix = struct

(* val condexToPostFix: Condex.pgm -> PostFix.pgm *)
fun condexToPostFix (Condex.Condex(numargs, body)) =
  PostFix.PostFix(numargs, expToCmds body 0)
  (* 0 is the initial value of a depth argument that statically 
     tracks how many values are on stack above the arguments *)

(* val expToCmds: Condex.exp -> int -> PostFix.cmd list *)
and expToCmds (Condex.Int i) depth = [PostFix.Int i]
  | expToCmds (Condex.Arg index) depth =
    [PostFix.Int (index + depth), PostFix.Nget]
    (* specified argument is on stack at index + depth *)
  | expToCmds (Condex.ArithApp(arithop,exp1,exp2)) depth = 
    (expToCmds exp1 depth) (* 1st operand is at same depth as whole binapp *)
    @ (expToCmds exp2 (depth + 1)) (* for 2nd operand, add 1 to depth to account for 1st operand *)
    @ [arithopToCmd arithop]
  | expToCmds (Condex.RelApp(relop,exp1,exp2)) depth =
    [PostFix.Int 11] (* replace this stub *)
  | expToCmds (Condex.And(exp1,exp2)) depth =
    [PostFix.Int 29] (* replace this stub *)
  | expToCmds (Condex.Or(exp1,exp2)) depth =
    [PostFix.Int 53] (* replace this stub *)
  | expToCmds (Condex.Ifnz(testExp,thenExp,elseExp)) depth =
    [PostFix.Int 87] (* replace this stub *)

(* put type here! *)    
and arithopToCmd Condex.Add = PostFix.Arithop PostFix.Add
  | arithopToCmd Condex.Sub = PostFix.Arithop PostFix.Sub
  | arithopToCmd Condex.Mul = PostFix.Arithop PostFix.Mul
  | arithopToCmd Condex.Div = PostFix.Arithop PostFix.Div
  | arithopToCmd Condex.Rem = PostFix.Arithop PostFix.Rem

and relopToCmd Condex.Lt = PostFix.Relop PostFix.Lt
  | relopToCmd Condex.Eq = PostFix.Relop PostFix.Eq
  | relopToCmd Condex.Gt = PostFix.Relop PostFix.Gt

(*****************************************************************************
  Test compiling condex programs from files
 *****************************************************************************)

datatype tests = IT of (int list * int) list (* IT means IntTests *)
	       | ST of (int list * string) list (* ST means StringTests *)

datatype maybePostFixPgm = PGM of PostFix.pgm 
			 | NO_PGM of string (* error message from failed translation *)
					   
fun testCompiler (condexFileName, testCases) =
  let val _ = print ("-------------------------------------------------\n"
		     ^ "Testing Condex program file "
		     ^ condexFileName ^ "\n")
      val condexPgm = Condex.fileToPgm(condexFileName)
      val _ = print ("Condex program input to compiler:\n"
		     ^ (Condex.pgmToString condexPgm) ^ "\n\n")
      val maybePFPgm = PGM(condexToPostFix(condexPgm))
		       handle exn => NO_PGM("No compiled PostFix program because: " 
					     ^ (exnName exn) ^ ": " ^ (exnMessage exn))
      val _ = let val msg = (case maybePFPgm of 
				 NO_PGM(errMsg) => errMsg
			       | PGM(pfPgm) => ("PostFix program output of compiler:\n"
						^ (PostFix.pgmToString pfPgm)))
	      in print(msg ^ "\n\n")
	      end
      val argsExpectedStringTuples =
	  (case testCases of
	       (IT argsExpectedIntTuples) =>
	       map (fn(args,expInt) => (args, Int.toString(expInt)))
		   argsExpectedIntTuples
	    |  (ST argsExpectedStrTuples) => argsExpectedStrTuples)
      fun testBehavior(args,expectedString) =
	  let val _ = print ("Testing args "
			     ^ (Utils.intsToString args) ^ ": ")
	      val condexResult =
		  Int.toString (Condex.run condexPgm args)
		  handle Condex.SyntaxError msg =>
			 ("Syntax error: " ^ msg)
		       | Condex.EvalError msg =>
			 ("Eval error: " ^ msg)
		       | exn => (exnName exn) ^ ": "
				^ (exnMessage exn)
	      val postfixResult =
		  (case maybePFPgm of 
		       NO_PGM(errMsg) => errMsg
		    | PGM(postfixPgm)  => 
		      Int.toString (PostFix.run postfixPgm args)
		      handle PostFix.SyntaxError msg =>
			     ("Syntax error: " ^ msg)
			   | PostFix.ExecError msg =>
			     ("Eval error: " ^ msg)
			   | exn => (exnName exn) ^ ": "
				    ^ (exnMessage exn))
	  in if condexResult = postfixResult then
		 if postfixResult = expectedString then
		     (print("both programs return expected result "
			    ^ expectedString ^ "\n");
		      (1,0)) (* 1 pass, 0 fails *)
		 else
		     (print("***ERROR IN BEHAVIOR***:"
			    ^"\nboth programs return same result, "
			    ^ "but it is not the expected one!"
			    ^ "\n  Program results: " ^ condexResult
			    ^ "\n  Expected result: " ^ expectedString
			    ^ "\n");
		      (0,1)) (* 0 passes, 1 fail *)
	     else
		 (print ("***ERROR IN TRANSLATION***"
			 ^ "\nprograms return different results:"
			 ^ "\n  Condex result: " ^ condexResult
			 ^ "\n PostFix result: " ^ postfixResult
			 ^ "\n");
		  (0,1)) (* 0 passes, 1 fail *)

	  end
  in combinePassesFails(List.map testBehavior argsExpectedStringTuples)
  end

and combinePassesFails passFails =
    foldl (fn ((passes, fails), (sumPasses, sumFails)) =>
	      (passes + sumPasses, fails + sumFails))
	  (0,0)
	  passFails
					 
val compilerTestCases = [
  ("avg.cdx", IT [([3,7], 5), ([15,5],10)]),
  ("andTest.cdx", IT [([0,0],0),([0,1],0),([1,0],0),([1,1],1),
                      ([0,2],0),([3,0],0),([4,5],5)]),
  ("orTest.cdx", IT [([0,0],0),([0,1],1),([1,0],1),([1,1],1),
                      ([0,2],2),([3,0],3),([4,5],4)]),
  ("sumRelations.cdx", IT [([5,6,5,4],3),
			   ([5,3,5,4],2), ([5,6,3,4],2), ([5,6,5,7],2),
			   ([5,6,7,7],1), ([5,3,5,7],1), ([5,3,7,4],1),
			   ([5,3,4,7],0)]),
  ("abs.cdx", IT [([~5],5), ([3],3), ([0],0)]),
  ("simpleIf.cdx", IT [([3,4],7), ([3,3],9), ([4,3],12)]),
  ("min3.cdx", IT [([4,5,6],4), ([4,6,5],4), 
		   ([5,4,6],4), ([5,6,4],4),
		   ([6,4,5],4), ([6,5,4],4)]), 
  ("nestedIfs.cdx", IT [([6,7,7],49), ([4,17,6],5), ([6,5,5],25), 
			([20,17,5],2), ([8,17,5],3), ([7,6,16],10)]),
  ("nonbooleanIfnzTest.cdx", IT [([0,1],0),([~1,1],~2),([3,4],12),([5,~6],~30)]),
  ("prob3a-g.cdx", IT [([10,2,8],7), ([11,2,8],~102), ([12,3,8],8), 
		       ([~7,2,3],38), ([5,4,5],~40)]),
  ("divRemTest.cdx", ST [([5,8,0],"3"),([6,17,0],"5"),
			 ([0,~8,2],"~4"), ([0,~18,3],"~6"),
			 ([3,2,0], "Eval error: Tried to divide 2 by 0"),
			 ([0,4,5], "Eval error: Tried to remainder 4 by 0")]),
  ("shortCircuitAndTest.cdx", ST [([0,2],"0"),([0,0],"0"),([4,5],"3"),
				  ([6,0],"Eval error: Tried to divide 17 by 0")]),
  ("shortCircuitOrTest.cdx", ST [([0,9],"6"),([4,5],"4"),([7,0],"7"),
				 ([0,0],"Eval error: Tried to remainder 42 by 0")])
]

fun testCompilerAll() =
    let val (passes, fails) =
	    combinePassesFails (List.map testCompiler
					 compilerTestCases)
    in if fails = 0 then
	   "Passed all " ^ (Int.toString passes) ^ " test cases"
       else
	   "Failed " ^ (Int.toString fails) ^ " of "
	   ^ (Int.toString(passes+fails))
	   ^ " test cases; for details, search above for ***ERROR."
    end
end (* End struct CondexToPostFix *)

fun testComp() = CondexToPostFix.testCompilerAll();

(* Execute this after loading: 

   testComp();				

 *)

