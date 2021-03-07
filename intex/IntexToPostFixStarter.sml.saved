(* Load PostFix and Intex structures *)
use "../postfix/PostFix.sml"; (* semi-colon required! *)
use "../intex/Intex.sml"; (* semi-colon required! *)
(* Note that structures overlap for constructors Int, Add, Sub, Mul, Div, Rem
   and type pgm *)

structure IntexToPostFix =

struct

(* intexToPostFix: Intex.pgm -> PostFix.pgm *)
fun intexToPostFix (Intex.Intex(numargs, exp)) =
  PostFix.PostFix(numargs, expToCmds exp 17)

(* expToCmds: exp -> int -> cmd list *)
and expToCmds (Intex.Int i) depth = []
  | expToCmds (Intex.Arg index) depth = []
  | expToCmds (Intex.BinApp(binop,exp1,exp2)) depth = []

(* binopToArithop : binop -> arithop *)
and binopToArithop Intex.Add = PostFix.Add
  | binopToArithop Intex.Sub = PostFix.Sub
  | binopToArithop Intex.Mul = PostFix.Mul
  | binopToArithop Intex.Div = PostFix.Div
  | binopToArithop Intex.Rem = PostFix.Rem

fun translateString intexPgmString =
  PostFix.pgmToString (intexToPostFix (Intex.stringToPgm intexPgmString))

end

open PostFix
open Intex
open IntexToPostFix

val intexP1 = Intex(0, BinApp(Mul,
			      BinApp(Sub, Int 7, Int 4),
			      BinApp(Div, Int 8, Int 2)))

val intexP1Test = Intex.run intexP1 []
val pfIntexP1 = intexToPostFix(intexP1)
val pfIntexP1Test = PostFix.run pfIntexP1 []

val intexP2 = Intex(4, BinApp(Mul,
			      BinApp(Sub, Arg 1, Arg 2),
			      BinApp(Div, Arg 3, Arg 4)))

val intexP2Test = Intex.run intexP2 [7, 4, 8, 2]
val pfIntexP2 = intexToPostFix(intexP2)
val pfIntexP2Test = PostFix.run pfIntexP2 [7, 4, 8, 2]

val pfSqr = intexToPostFix(sqr)
val pfAvg = intexToPostFix(avg)
val pfF2C = intexToPostFix(f2c)

val pfSqrTest = PostFix.run pfSqr [5]
val pfAvgTest = PostFix.run pfAvg [5, 15]
val pfF2CTest = PostFix.run pfF2C [86]

