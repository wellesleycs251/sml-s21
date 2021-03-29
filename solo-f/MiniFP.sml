(***************************************************************
 MiniFP is an implementation of a subset of the FP language
 from Backus's Turing Award paper. 
 ****************************************************************)

use "../sexp/Sexp.sml";

structure MiniFP = struct

  exception SyntaxError of string

  (************************************************************
   Abstract Syntax
   ************************************************************)

  datatype obj = 
      Int of int (* integer objects *)
    | Seq of obj list (* sequence of objects *)

  and funForm = 
      Id    (* identity *)
    | Add   (* addition *)
    | Sub   (* subtraction *)
    | Mul   (* multiplication *) 
    | Div   (* division *)
    | Distl (* distribute from left *)
    | Distr (* distribute from right *)
    | Trans (* transpose *)
    | Const of obj  (* constant function *)
    | Sel of int    (* selection *)
    | Map of funForm  (* alpha = map *)
    | Reduce of funForm (* / = reduce *)
    | BinaryToUnary of funForm * obj (* bu *)
    | Compose of funForm * funForm  (* o = composition *)
    | FunSeq of funForm list (* [...] = function sequence *)
			
  (************************************************************
   Parsing from S-Expressions
   ************************************************************)

  fun sexpToObj (Sexp.Int i) = Int i
    | sexpToObj (Sexp.Seq sexps) = Seq (List.map sexpToObj sexps)
    | sexpToObj sexp = raise (SyntaxError ("invalid MiniFP object " ^ (Sexp.sexpToString sexp)))

  and stringToObj s = sexpToObj (Sexp.stringToSexp s)

  fun sexpToFunForm (Sexp.Sym "id") = Id
    | sexpToFunForm (Sexp.Sym "+") = Add
    | sexpToFunForm (Sexp.Sym "-") = Sub
    | sexpToFunForm (Sexp.Sym "x") = Mul
    | sexpToFunForm (Sexp.Sym "-:") = Div
    | sexpToFunForm (Sexp.Sym "distl") = Distl
    | sexpToFunForm (Sexp.Sym "distr") = Distr
    | sexpToFunForm (Sexp.Sym "trans") = Trans
    | sexpToFunForm (Sexp.Int i) = Sel i
    | sexpToFunForm (Sexp.Seq[Sexp.Sym "$", sx]) = Const (sexpToObj sx)
    | sexpToFunForm (Sexp.Seq[Sexp.Sym "a", sx]) = Map (sexpToFunForm sx)
    | sexpToFunForm (Sexp.Seq[Sexp.Sym "/", sx]) = Reduce (sexpToFunForm sx)
    | sexpToFunForm (Sexp.Seq[Sexp.Sym "bu", sx1, sx2])
      = BinaryToUnary (sexpToFunForm sx1, sexpToObj sx2)
    | sexpToFunForm (Sexp.Seq[Sexp.Sym "o", sx1, sx2])
      = Compose (sexpToFunForm sx1, sexpToFunForm sx2)
    (*		       
    (* Paola Boettner's nice foldr version for "o" composition *)
    | sexpToFunForm (Sexp.Seq((Sexp.Sym "o") :: sxs)) = 
      foldr (fn (sx,ff) => Compose(sexpToFunForm sx, ff)) Id sxs
     *)
    | sexpToFunForm (Sexp.Seq sxs) = FunSeq (List.map sexpToFunForm sxs)
    | sexpToFunForm sexp =
      raise (SyntaxError ("invalid MiniFP functional form " ^ (Sexp.sexpToString sexp)))

  and stringToFunForm s = sexpToFunForm (Sexp.stringToSexp s)

  (************************************************************
   Unparsing to S-Expressions
   ************************************************************)

  fun objToSexp (Int i) = Sexp.Int i
    | objToSexp (Seq xs) = Sexp.Seq (List.map objToSexp xs)

  and objToString obj = Sexp.sexpToString (objToSexp obj)

  fun funFormToSexp Id = Sexp.Sym "id"
    | funFormToSexp Add = Sexp.Sym "+"
    | funFormToSexp Sub = Sexp.Sym "-"
    | funFormToSexp Mul = Sexp.Sym "x"
    | funFormToSexp Div = Sexp.Sym "-:"
    | funFormToSexp Distl = Sexp.Sym "distl"
    | funFormToSexp Distr = Sexp.Sym "distr"
    | funFormToSexp Trans = Sexp.Sym "trans"
    | funFormToSexp (Sel i) = Sexp.Int i
    | funFormToSexp (Const x) = Sexp.Seq[Sexp.Sym "$", objToSexp x]
    | funFormToSexp (Map ff) = Sexp.Seq[Sexp.Sym "a", funFormToSexp ff]
    | funFormToSexp (Reduce ff) = Sexp.Seq[Sexp.Sym "/", funFormToSexp ff]
    | funFormToSexp (BinaryToUnary(ff,x)) =
      Sexp.Seq[Sexp.Sym "bu", funFormToSexp ff,  objToSexp x]
    | funFormToSexp (Compose(ff1,ff2)) = 
      Sexp.Seq[Sexp.Sym "o",  funFormToSexp ff1, funFormToSexp ff2]
    | funFormToSexp (FunSeq ffs) = Sexp.Seq(List.map funFormToSexp ffs)

  and funFormToString ff = Sexp.sexpToString (funFormToSexp ff)

  (************************************************************
   Sample Objects and Functional Forms in Abstract Syntax
   ************************************************************)

  val vector1 = Seq[Int 2, Int 3, Int 5]

  val vector2 = Seq[Int 10, Int 20, Int 30]

  val vectors = Seq[vector1, vector2] (* A pair of vectors or a 2x3 matrix *)

  val matrix = Seq[Seq[Int 1, Int 4], Seq[Int 8, Int 6], Seq[Int 7, Int 9]] (* A 3x2 matrix *)
      
  val matrices1 = Seq[vectors, matrix] (* A pair of a 2x3 matrix and a 3x2 matrix *)
      
  val matrices2 = Seq[matrix, vectors] (* A pair of a 3x2 matrix and a 2x3 matrix *)

  val vector1_sx = Sexp.stringToSexp("(2 3 5)")

  val vector2_sx = Sexp.stringToSexp("(10 20 30)")

  val vectors_sx = Sexp.Seq([vector1_sx, vector2_sx])

  val matrix_sx =  Sexp.stringToSexp("((1 4) (8 6) (7 9))")

  val matrices1_sx = Sexp.Seq([vectors_sx, matrix_sx])

  val matrices2_sx = Sexp.Seq([matrix_sx, vectors_sx])

  (* inner product example from Backus's paper *)
  val IP = Compose(Reduce Add, Compose(Map Mul, Trans))

  (* matrix multiply example from Backus's paper *)
  val MM = Compose(Map(Map IP), 
		   Compose(Map Distl, 
			   Compose(Distr, 
				   FunSeq[Sel 1, Compose(Trans, Sel 2)])))

  (************************************************************
   Sample Objects and Functional Forms in S-Expression Syntax
  ************************************************************)

  val IP_sx = Sexp.stringToSexp "(o (/ +) (o (a x) trans))"

  val MM_sx = Sexp.stringToSexp "(o (a (a (o (/ +) (o (a x) trans))))\
                                 \  (o (a distl)                     \
                                 \     (o distr                      \
                                 \        (1 (o trans 2)))))"

  (************************************************************
   Put your solutions to PS10 Problem 1a here:
   ************************************************************)		  

  val F = Id (* replace this stub *)

  val F_sx = Sexp.stringToSexp "id" (* replace this stub *)

end 




