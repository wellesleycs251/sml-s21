use "../sexp/Sexp.sml";

structure Intex =

struct

(*****************************************************************************
 Sum-of-Product syntax for Intex
 *****************************************************************************)

datatype pgm = Intex of int * exp
     and exp = Int of int
             | Arg of int
	     | ArithApp of arithop * exp * exp
     and arithop = Add | Sub | Mul | Div | Rem

val sqr = Intex(1, ArithApp(Mul, Arg 1, Arg 1))
val avg = Intex(2, ArithApp(Div, ArithApp(Add, Arg 1, Arg 2), Int 2))
(* val f2c = Flesh out this Farenheit to Celsius converter! *)
val divRem = Intex(5, ArithApp(Add,
			       ArithApp(Mul,
					ArithApp(Div, Arg 1, Arg 2),
					Arg 3),
			       ArithApp(Rem, Arg 4, Arg 5)))

(*****************************************************************************
 Intex Interpreter
 *****************************************************************************)

(* val run: Intex.pgm -> int list -> int *)
fun run (Intex(numargs, exp)) args =
  17

(* val eval: Intex.exp -> int list -> int *)
and eval (Int i) args = 17
  | eval (Arg index) args = 23
  | eval (ArithApp(aop, exp1, exp2)) args = 42

(*   val arithopToFun: Intex.arithop -> int * int -> int 

   Recall -> is *right* associative, so this is equivalen to:

     val arithopToFun: Intex.arithop -> (int * int -> int)
 *)
and arithopToFun Add = op+
  | arithopToFun Mul = op*
  | arithopToFun Sub = op-
  | arithopToFun Div = (fn(x,y) => x div y)
  | arithopToFun Rem = (fn(x,y) => x mod y)

(*****************************************************************************
 Parsing from S-Expressions
 *****************************************************************************)

exception SyntaxError of string

(* val sexpToPgm : Sexp.sexp -> Intex.pgm *)
fun sexpToPgm (Sexp.Seq[Sexp.Sym "intex", Sexp.Int n, body]) =
    Intex(n, sexpToExp body)
  | sexpToPgm sexp = raise (SyntaxError ("invalid Intex program: "
					 ^ (Sexp.sexpToString sexp)))

(* val sexpToExp : Sexp.sexp -> Intex.exp *)
and sexpToExp (Sexp.Int i) = Int i
  | sexpToExp (Sexp.Seq[Sexp.Sym "$", Sexp.Int i]) = Arg i
  (* Treat $n as a shorthand for ($ n) *)
  | sexpToExp (Sexp.Sym s) =
    if String.sub(s, 0) = #"$" then
	(* try to parse rest of string as int *)
	(case Int.fromString(String.extract(s, 1, NONE)) of
	     SOME i => Arg i
           | NONE => raise (SyntaxError ("invalid Intex symbol " ^ s)))
	handle
    	  exn => raise (SyntaxError ("Exception when parsing Intex symbol: "
	  			     ^ (exnMessage exn)))
    else
	raise (SyntaxError ("invalid Intex symbol" ^ s))
  | sexpToExp (Sexp.Seq[Sexp.Sym p, rand1, rand2]) =
    ArithApp(stringToPrimop p, sexpToExp rand1, sexpToExp rand2)
  | sexpToExp sexp =  raise (SyntaxError ("invalid Intex expression: "
					  ^ (Sexp.sexpToString sexp)))

(* val stringToPrimop : string -> Intex.arithop *)
and stringToPrimop "+" = Add
  | stringToPrimop "-" = Sub
  | stringToPrimop "*" = Mul
  | stringToPrimop "/" = Div
  | stringToPrimop "%" = Rem
  | stringToPrimop s = raise (SyntaxError ("invalid Intex primop: " ^ s))

(* val stringToExp : string -> Intex.exp *)
and stringToExp s = sexpToExp (Sexp.stringToSexp s)

(* val stringToPgm : string -> Intex.pgm *)
and stringToPgm s = sexpToPgm (Sexp.stringToSexp s)

(*****************************************************************************
 Unparsing to S-Expressions 
 *****************************************************************************)

(* val pgmToSexp : Intex.pgm -> Sexp.sexp *)
fun pgmToSexp (Intex(n,body)) =
  Sexp.Seq[Sexp.Sym "intex", Sexp.Int n, expToSexp body]

(* val expToSexp : Intex.exp -> Sexp.sexp *)
and expToSexp (Int i) = Sexp.Int i
  | expToSexp (Arg i) = Sexp.Seq[Sexp.Sym "$", Sexp.Int i]
  | expToSexp (ArithApp (rator, rand1, rand2)) =
    Sexp.Seq[Sexp.Sym (primopToString rator),
	     expToSexp rand1, expToSexp rand2]

(* val primopToString : Intex.arithop -> string *)
and primopToString Add = "+"
    | primopToString Sub = "-"
    | primopToString Mul = "*"
    | primopToString Div = "/"
    | primopToString Rem = "%"

(* val expToString : Intex.exp -> string *)
and expToString s = Sexp.sexpToString (expToSexp s)

(* val pgmToString : Intex.pgm -> string *)
and pgmToString s = Sexp.sexpToString (pgmToSexp s)			      

end

(*****************************************************************************
 Testing with Sum-of-Product programs
 *****************************************************************************)

(* open Intex structure when loading so all functions available unqualified *)
open Intex

(* increase default printDepth, printLength, stringDepth *)
val _ = Control.Print.printDepth := 10000;
val _ = Control.Print.printLength := 10000;
val _ = Control.Print.stringDepth := 10000;

val sqrTest = run sqr [5]
val avgTest = run avg [5,15]
(*
val f2cTests = map (fn temp => (temp, run f2c temp)) 
                   [[~40], [0], [32], [86], [98], [212]] 
 *)

(* val testRun = fn : pgm -> int list -> string *)
fun testRun pgm args =
  Int.toString (run pgm args) (* Convert to string so same type as error messages below *)
  handle exn => "Exception raised: " ^ (exnMessage exn)

(*****************************************************************************
 Testing with S-Expression programs 
 *****************************************************************************)

exception SexpError of string * Sexp.sexp					      

(* testRun' takes sexpStrings instead *)
(* val testRun' = fn : string -> string -> string *)
fun testRun' pgmSexpString argsSexpString =				     
    testRun (stringToPgm pgmSexpString)
	    (sexpStringToIntList argsSexpString)
    handle SexpError (msg, sexp) => ("SexpError: " ^ msg ^ " " ^ (Sexp.sexpToString sexp))
         | Sexp.IllFormedSexp msg => ("SexpError: Ill-formed sexp " ^ msg)
         | other => "Unknown exception: " ^ (exnMessage other)

(* val sexpStringToIntList = fn : string -> int list *)
and sexpStringToIntList str =
    let val sexp = Sexp.stringToSexp str
    in case sexp of
	   Sexp.Seq xs => List.map sexpToInt xs
	 | _  => raise SexpError("expected sexp sequence but got", sexp)
    end

(* val sexpToInt = fn : Sexp.sexp -> int *)
and sexpToInt (Sexp.Int i) = i
  | sexpToInt sexp = raise SexpError("expected sexp int but got", sexp)

val avgTest2 = testRun' "(intex 2 (/ (+ ($ 1) ($ 2)) 2))" "(5 15)"

(*
val f2cTest2 = List.map (testRun' "(intex 1 (/ ( * (- ($ 1) 32) 5) 9))")
			["(-40)", "(0)", "(32)", "(86)", "(98)", "(212)"]
*)
