(* Load PostFix and Intex structures *)
use "../postfix/PostFix.sml"; (* semi-colon required! *)
use "../intex/Intex.sml"; (* semi-colon required! *)
(* Note that structures overlap for constructors Int, Add, Sub, Mul, Div, Rem
   and type pgm *)

structure IntexToPostFix =

struct

(*****************************************************************************
 Intex to PostFix compiler
 *****************************************************************************)

(* intexToPostFix: Intex.pgm -> PostFix.pgm *)
fun intexToPostFix (Intex.Intex(numargs, exp)) =
  PostFix.PostFix(numargs, expToCmds exp 0)
    (* 0 is a depth argument that statically tracks 
       how many values are on stack above the arguments *)

(* expToCmds: exp -> int -> cmd list *)
and expToCmds (Intex.Int i) depth = [PostFix.Int i]
  | expToCmds (Intex.Arg index) depth = [PostFix.Int (index + depth), PostFix.Nget]
   (* specified argument is on stack at index + depth *)
  | expToCmds (Intex.ArithApp(aop,exp1,exp2)) depth =
    (expToCmds exp1 depth) (* 1st operand is at same depth as whole binapp *)
    @ (expToCmds exp2 (depth + 1)) (* for 2nd operand, add 1 to depth to account for 1st operand *)
    @ [PostFix.Arithop (arithopToArithop aop)]

(* arithopToArithop : arithop -> arithop *)	  
and arithopToArithop Intex.Add = PostFix.Add
  | arithopToArithop Intex.Sub = PostFix.Sub
  | arithopToArithop Intex.Mul = PostFix.Mul
  | arithopToArithop Intex.Div = PostFix.Div
  | arithopToArithop Intex.Rem = PostFix.Rem

fun translateString intexPgmString =
  PostFix.pgmToString (intexToPostFix (Intex.stringToPgm intexPgmString))

end

(*****************************************************************************
 Testing the Intex to PostFix compiler
 *****************************************************************************)

(* Open these so we can use their values without qualified names. 
   Note that Int will mean Intex.Int, Add will mean Intex.Add, 
     run will mean Intex.run, etc. 
   PostFix versions of these will need explicit qualification:
     PostFix.Int, PostFix.Add, PostFix.run, etc. 
 *)

open Intex
open IntexToPostFix

val msg1 = "Defining Intex program intexP1"
val intexP1 = Intex(0, ArithApp(Mul,
				ArithApp(Sub, Int 7, Int 4),
				ArithApp(Div, Int 8, Int 2)))

val msg2 = "intexP1 as an sexp string:"
val intexP1String = pgmToString(intexP1)

val msg4 = "Translating Intex program intexP1 to PostFix program pfIntexP1"
val pfIntexP1 = intexToPostFix(intexP1)

val msg5 = "pfIntexP1 as an sexp string:"
val pfIntexP1String = PostFix.pgmToString(pfIntexP1)

val msg3 = "Running intexP1 in Intex on arglist []"
val intexP1Test = Intex.run intexP1 []

val msg6 = "Running pfIntexP1 in PostFix on arglist []"
val pfIntexP1Test = PostFix.run pfIntexP1 []

val msg7 = "Defining Intex program intexP2"
val intexP2 = Intex(4, ArithApp(Mul,
				ArithApp(Sub, Arg 1, Arg 2),
				ArithApp(Div, Arg 3, Arg 4)))

val msg8 =  "intexP2 as an sexp string:"
val intexP2String = pgmToString(intexP2)

val msg9 = "Translating Intex program intexP2 to PostFix program pfIntexP2"
val pfIntexP2 = intexToPostFix(intexP2)

val msg10 = "pfIntexP2 as an sexp string:"
val pfIntexP2String = PostFix.pgmToString(pfIntexP2)

val msg11  = "Running intexP2 in Intex on arglist [7,4,8,2]"
val intexP2Test = Intex.run intexP2 [7,4,8,2]

val msg12 = "Running pfIntexP2 in PostFix on arglist []"
val pfIntexP2Test = PostFix.run pfIntexP2 [7,4,8,2]

val msg13 = "Intex program sqr as an sexp string"
val sqrString = pgmToString(sqr)

val msg14 = "Translating Intex program sqr to PostFix program pfSqr"
val pfSqr = intexToPostFix(sqr)

val msg15 = "pfSqr as an sexp string:"
val pfSqrString = PostFix.pgmToString(pfSqr)

val msg16 = "testing: val pfSqrTest = PostFix.run pfSqr [5]"
val pfSqrTest = PostFix.run pfSqr [5]

val msg17 = "Intex program avg as an sexp string"
val avgString = pgmToString(avg)

val msg18 = "Translating Intex program avg to PostFix program pfAvg"
val pfAvg = intexToPostFix(avg)

val msg19 = "pfAvg as an sexp string:"
val pfAvgString = PostFix.pgmToString(pfAvg)

val msg20 = "testing: val pfAvgTest = PostFix.run pfAvg [5, 15]"
val pfAvgTest = PostFix.run pfAvg [5, 15]

val msg21 = "Intex program f2c as an sexp string"
val f2cString = pgmToString(f2c)

val msg22 = "Translating Intex program f2c to PostFix program pfF2c"
val pfF2c = intexToPostFix(f2c)

val msg23 = "pfF2c as an sexp string:"
val pfF2cString = PostFix.pgmToString(pfF2c)

val msg24 = "testing: val pfF2cTest = PostFix.run pfF2c [86]"
val pfF2cTest = PostFix.run pfF2c [86]

