use "../sexp/Sexp.sml";
use "../utils/Utils.sml";

(*******************************************************************************
 Condex adds a few constructs to Intex. 

 As in Intex, all values in Condex are integers. 

 Condex adds to Intex the following:

 1. The relational operators <, =, and >. 
    Since all Condex values are integers, these operators return 1 
    in the true case and 0 in the false case. This is similar to 
    relational operators in PostFix. 

 2. A conditional expression (ifnz E_test E_then E_else), 
    where `ifnz` stands for `if-non-zero` and is pronounced `iffenz`.

    The meaning of the ifnz expression is as follows:

    First, the E_test is evaluated to an integer value V_test.
    + If V_test is non-zero, then E_then is evaluated, and its value is
      returned as the value of the ifnz expression 
    + If V_test is zero, then E_else is evaluated, and its value is
      returned as the value of the ifnz expression 
    + If an error is encountered during any of these evaluations,
       evaluating the ifnz expression gives the same error 

    As with Racket's if expresssion, Condex's ifnz evaluates at 
    most two subexpressions. E_test is always evaluated, and if that
    evaluation does not give an error, exactly one of E_then or
    E_else is evaluated.

****************************************************************)

structure Condex =

struct

(*****************************************************************************
 Sum-of-Product syntax for Condex
 *****************************************************************************)

datatype pgm = Condex of int * exp
     and exp = Int of int
             | Arg of int
	     | ArithApp of arithop * exp * exp 
	     | RelApp of relop * exp * exp (* new in Condex *)
             | And of exp * exp (* short-circuit conjunction *)
             | Or of exp * exp (* short-circuit disjunction *)
	     | Ifnz of exp * exp * exp (* new in Condex: E_test, E_then, E_else *)
     and arithop = Add | Sub | Mul | Div | Rem 
     and relop = Lt | Eq | Gt (* <, =, > *)

(* Example: Return the absolute value of the argument *)
val abs = Condex(1, (Ifnz(RelApp(Lt, Arg 1, Int 0), 
                          ArithApp(Sub, Int 0, Arg 1), 
			  Arg 1)))


(*****************************************************************************
 Condex Interpreter
 *****************************************************************************)

exception EvalError of string

(* val run: Condex.pgm -> int list -> int *)
fun run (Condex(numargs, exp)) args =
  if numargs <> List.length args
  then raise EvalError "Mismatch between expected and actual number of args"
  else eval exp args

(* val eval: Condex.exp -> int list -> int *)
and eval (Int i) args = i
  | eval (Arg index) args = 
    if (index <= 0) orelse (index > List.length args)
    then raise EvalError "Arg index out of bounds"
    else List.nth(args, index-1)
  | eval (ArithApp(arithop, exp1, exp2)) args =
    let val i1 = eval exp1 args
	val i2 = eval exp2 args
    in (case (arithop, i2) of
	    (Div, 0) => raise EvalError "Division by 0"
	  | (Rem,0) => raise EvalError "Remainder by 0"
	  | _ => (arithopToFun arithop)(i1, i2))
    end
  | eval (RelApp(relop, exp1, exp2)) args =
    23 (* replace this stub *)
  | eval (And(exp1, exp2)) args =
    57 (* replace this stub *)
  | eval (Or(exp1, exp2)) args =
    71 (* replace this stub *)
  | eval (Ifnz(testExp,thenExp,elseExp)) args =
    97 (* replace this stub *)

(* Named changed from Intex interpreter's binopToFun *)	      
and arithopToFun Add = op+
  | arithopToFun Mul = op*
  | arithopToFun Sub = op-
  | arithopToFun Div = 
    (fn(x,y) => if y = 0 then 
		  raise EvalError ("Tried to divide " ^ (Int.toString x) ^ " by 0")
		else
		  x div y)
  | arithopToFun Rem = 
    (fn(x,y) => if y = 0 then 
		  raise EvalError ("Tried to remainder " ^ (Int.toString x) ^ " by 0")
		else
		  x mod y)

(* New in Condex interpreter *)			   
and relopToFun Lt = op<
  | relopToFun Eq = (fn(a,b) => a = b)
  | relopToFun Gt = op>

and boolToInt false = 0
  | boolToInt true = 1

(*****************************************************************************
 Parsing from S-Expressions
 *****************************************************************************)

exception SyntaxError of string

fun sexpToPgm (Sexp.Seq[Sexp.Sym "condex", Sexp.Int n, bodyx]) =
    Condex(n, sexpToExp bodyx)
  | sexpToPgm sexp = raise (SyntaxError ("invalid Condex program: "
					 ^ (Sexp.sexpToString sexp)))

and sexpToExp (Sexp.Int i) = Int i
  | sexpToExp (Sexp.Seq[Sexp.Sym "$", Sexp.Int i]) = Arg i
  (* Treat $n as a shorthand for ($ n) *)
  | sexpToExp (Sexp.Sym s) =
    if String.sub(s, 0) = #"$" then
	(* try to parse rest of string as int *)
	(case Int.fromString(String.extract(s, 1, NONE)) of
	     SOME i => Arg i
           | NONE => raise (SyntaxError ("invalid Condex symbol " ^ s)))
	handle
    	  exn => raise (SyntaxError ("Exception when parsing Condex symbol: "
	  			     ^ (exnMessage exn)))
    else
	raise (SyntaxError ("invalid Condex symbol" ^ s))
  | sexpToExp (Sexp.Seq[Sexp.Sym opname, rand1x, rand2x]) =
    if Utils.listMember opname ["<", "=", ">"] then
      RelApp(stringToRelop opname, sexpToExp rand1x, sexpToExp rand2x)
    else if Utils.listMember opname ["+", "-", "*", "/", "%"] then
      ArithApp(stringToArithop opname, sexpToExp rand1x, sexpToExp rand2x)
    else if opname = "and" then 
      And(sexpToExp rand1x, sexpToExp rand2x)  
    else if opname = "or" then 
      Or(sexpToExp rand1x, sexpToExp rand2x)  
    else
	raise (SyntaxError ("invalid Condex operator: " ^ opname))
  | sexpToExp (Sexp.Seq[Sexp.Sym "ifnz", tstx, thnx, elsx]) =
    Ifnz(sexpToExp tstx, sexpToExp thnx, sexpToExp elsx)
  | sexpToExp sexp =  raise (SyntaxError ("invalid Condex expression: "
					  ^ (Sexp.sexpToString sexp)))

and stringToArithop "+" = Add
  | stringToArithop "-" = Sub
  | stringToArithop "*" = Mul
  | stringToArithop "/" = Div
  | stringToArithop "%" = Rem
  | stringToArithop s = raise (SyntaxError ("invalid Condex arithop: " ^ s))

and stringToRelop "<" = Lt
  | stringToRelop "=" = Eq
  | stringToRelop ">" = Gt
  | stringToRelop s = raise (SyntaxError ("invalid Condex relop: " ^ s))

(* val stringToExp : string -> exp *)			     
and stringToExp s = sexpToExp (Sexp.stringToSexp s)

(* val stringToPgm : string -> pgm *)			      
and stringToPgm s = sexpToPgm (Sexp.stringToSexp s)

(* val fileToPgm : string -> pgm *)
and fileToPgm filename = sexpToPgm (Sexp.fileToSexp filename)

(*****************************************************************************
 Unparsing to S-Expressions 
 *****************************************************************************)

fun pgmToSexp (Condex(n,body)) =
  Sexp.Seq[Sexp.Sym "condex", Sexp.Int n, expToSexp body]

and expToSexp (Int i) = Sexp.Int i
  | expToSexp (Arg i) = Sexp.Seq[Sexp.Sym "$", Sexp.Int i]
  | expToSexp (ArithApp (rator, rand1, rand2)) =
    Sexp.Seq[Sexp.Sym (arithopToString rator),
	     expToSexp rand1, expToSexp rand2]
  | expToSexp (RelApp (rator, rand1, rand2)) =
    Sexp.Seq[Sexp.Sym (relopToString rator),
	     expToSexp rand1, expToSexp rand2]
  | expToSexp (And(rand1, rand2)) =
    Sexp.Seq[Sexp.Sym "and", expToSexp rand1, expToSexp rand2]
  | expToSexp (Or(rand1, rand2)) =
    Sexp.Seq[Sexp.Sym "or", expToSexp rand1, expToSexp rand2]
  | expToSexp (Ifnz (testExp,thenExp,elseExp)) =
    Sexp.Seq[Sexp.Sym "ifnz", expToSexp testExp,
	     expToSexp thenExp, expToSexp elseExp]

and arithopToString Add = "+"
    | arithopToString Sub = "-"
    | arithopToString Mul = " *" (* Put space before * so doesn't look like SML comment *)
    | arithopToString Div = "/"
    | arithopToString Rem = "%"

and relopToString Lt = "<"
  | relopToString Eq = "="
  | relopToString Gt = ">"

and expToString s = Sexp.sexpToString (expToSexp s)
and pgmToString s = Sexp.sexpToString (pgmToSexp s)

(*****************************************************************************
 Repl 
 *****************************************************************************)

				      
(* An interactive read-eval-print loop (REPL) for Condex expressions.
   By default, assumes zero arguments, but this can be changed
   with the #args directive (see below). The following directives
   are supported:

   + (#args i_1 i_n): Installs the n integers i_ 1 ... i_n 
      as the current positional program arguments

   + (#run <pgm> <arg1> ... <arg_n>) runs the program specified
     by <filename> on the arguments in <args>, where

     - <pgm> is a symbol or string naming a file containing the program
       or an sexp representation of the program.

    - <arg_i> are integer program arguments

      E.g., (#run avg.itx 5 15)
            (#run "avg.itx" 5 15)
            (#run (condex 2 (/ (+ ($ 1) ($ 2)) 2)) 5 15)

     + (#quit): Exit the interpreter
 *)

fun repl () =
  let
    fun println s = print (s^"\n")

    (* sexpToInt : sexp -> int *)
    fun sexpToInt (Sexp.Int i) = i
      | sexpToInt sexp = raise (Fail ("Not an int!: " ^ (Sexp.sexpToString sexp)))
    (* getPgm : sexp -> pgm *)				    
    (* get a Bindex program from a specification *)
    (* treat a symbol or string as the name of a file containing the program *)
    fun getPgm (Sexp.Sym filename) = fileToPgm filename
      | getPgm (Sexp.Str filename) = fileToPgm filename
      | getPgm sexp = sexpToPgm sexp 

    fun loop ints = (* ints are positional arguments *)
      let val _ = print "\ncondex> " 
	  val sexp = Sexp.readSexp()
      in case sexp of 
	     Sexp.Seq [Sexp.Sym "#quit"] => println "Moriturus te saluto!"
	   | Sexp.Seq ((Sexp.Sym "#args") :: intxs) => 
	     let val args = List.map sexpToInt intxs
	     in loop args (* install args as new default arguments *)
	     end
	   | Sexp.Seq ((Sexp.Sym "#run") :: pgmx :: intxs) => 
	     let val _ = println (Int.toString (run (getPgm pgmx) (List.map sexpToInt intxs)))
			 handle EvalError s => println ("Error: " ^ s)
			      | SyntaxError s => println ("Error: " ^ s)
			      | Fail s => println ("Error: " ^ s)
			      | other => println ("Error: " ^ (exnName other)
						  ^ " -- " ^ (exnMessage other))
	     in loop ints (* use same default ints as before *)
	     end
	   (* Otherwise evaluate expression relative to current arguments
              determined by #args *)      
	   | _ => let val  _ = println (Int.toString (eval (sexpToExp sexp) ints))
			       handle EvalError s => println ("Error: " ^ s)
				    | SyntaxError s => println ("Error: " ^ s)
				    | Fail s => println ("Error: " ^ s)
				    | other => println ("Error: " ^ (exnName other)
							  ^ " -- " ^ (exnMessage other))
		  in loop ints (* use same default ints as before *)
		  end
      end
  in loop []
  end


(*	 
val sqrTest = run sqr [5]
val avgTest = run avg [5,15]
val f2cTest = run f2c [86]
 *)

fun testRun pgm args =
  Int.toString (run pgm args) (* Convert to string so same type as error messages below *)
  handle EvalError msg => "EvalError: " ^ msg
       | other => "Unknown exception: " ^ (exnMessage other)

exception SexpError of string * Sexp.sexp					      
(* testRun' takes sexpStrings instead *)
fun testRun' pgmSexpString argsSexpString =				     
    testRun (stringToPgm pgmSexpString)
	    (sexpStringToIntList argsSexpString)
    handle SexpError (msg, sexp) => ("SexpError: " ^ msg ^ " " ^ (Sexp.sexpToString sexp))
         | Sexp.IllFormedSexp msg => ("SexpError: Ill-formed sexp " ^ msg)
         | other => "Unknown exception: " ^ (exnMessage other)

and sexpStringToIntList str =
    let val sexp = Sexp.stringToSexp str
    in case sexp of
	   Sexp.Seq xs => List.map sexpToInt xs
	 | _  => raise SexpError("expected sexp sequence but got", sexp)
    end

and sexpToInt (Sexp.Int i) = i
  | sexpToInt sexp = raise SexpError("expected sexp int but got", sexp)

(* Test interpreting condex programs from files *)

datatype tests = IT of (int list * int) list (* IT means IntTests *)
	       | ST of (int list * string) list (* ST means StringTests *)
					   
fun testInterpreter (condexFileName, testCases) =
  let val _ = print ("-------------------------------------------------\n"
		     ^ "Testing Condex program file "
		     ^ condexFileName ^ "\n")
      val condexPgm = fileToPgm(condexFileName)
      val _ = print ("Condex program input to interpreter:\n"
		     ^ (pgmToString condexPgm) ^ "\n\n")
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
		  Int.toString (run condexPgm args)
		  handle SyntaxError msg =>
			 ("Syntax error: " ^ msg)
		       | EvalError msg =>
			 ("Eval error: " ^ msg)
		       | exn => (exnName exn) ^ ": "
				^ (exnMessage exn)
	  in if condexResult = expectedString
	     then (print ("Program returned expected result "
			  ^ condexResult ^ "\n");
		   (1,0)) (* Return 1 pass, 0 fails *)
	     else (print ("***ERROR***"
			 ^ "\n   Actual result: " ^ condexResult
			 ^ "\n Expected result: " ^ expectedString
			 ^ "\n"); 
		   (0,1)) (* Return 0 passes, 1 fail *)
	  end
  in combinePassesFails(List.map testBehavior argsExpectedStringTuples)
  end

and combinePassesFails passFails =
    foldl (fn ((passes, fails), (sumPasses, sumFails)) =>
	      (passes + sumPasses, fails + sumFails))
	  (0,0)
	  passFails

(* Testing *)
val interpreterTestCases = [
  ("avg.cdx", IT [([3,7],5), ([15,5],10)]),
  ("sumRelations.cdx", IT [([5,6,5,4],3),
			   ([5,3,5,4],2), ([5,6,3,4],2), ([5,6,5,7],2),
			   ([5,6,7,7],1), ([5,3,5,7],1), ([5,3,7,4],1),
			   ([5,3,4,7],0)]),
  ("andTest.cdx", IT [([0,0],0),([0,1],0),([1,0],0),([1,1],1),
                      ([0,2],0),([3,0],0),([4,5],5)]),
  ("orTest.cdx", IT [([0,0],0),([0,1],1),([1,0],1),([1,1],1),
                      ([0,2],2),([3,0],3),([4,5],4)]),
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

fun testInterpreterAll() =
    let val (passes, fails) =
	    combinePassesFails (List.map testInterpreter
					 interpreterTestCases)
    in if fails = 0 then
	   "Passed all " ^ (Int.toString passes) ^ " test cases"
       else
	   "Failed " ^ (Int.toString fails) ^ " of "
	   ^ (Int.toString(passes+fails))
	   ^ " test cases; for details, search above for ***ERROR***."
    end

end (* End struct Condex *)

val testRun = Condex.testRun;
val testRun' = Condex.testRun';
val repl = Condex.repl;
fun testInterp() = Condex.testInterpreterAll();

(* Execute this after loading: 

   testInterp();				

 *)

			

			
			   


			

			
			   

