use "../solo-f/MiniFP.sml";

structure MiniFPInterp = struct
  
  open MiniFP

  exception EvalError of string

  (* apply: funForm -> obj -> obj *)
  fun apply Id obj = obj (* Correct Id implementation *)
    | apply Add (Seq[Int x, Int y]) = Int(x+y) (* Correct Add implementation *)
    | apply Sub obj = obj (* Replace this stub *)
    | apply Mul obj = obj (* Replace this stub *)
    | apply Div obj = obj (* Replace this stub. *)
    | apply Distl obj = obj (* Replace this stub. *)
    | apply Distr obj = obj (* Replace this stub. *)
    | apply Trans (Seq xs) = Seq(transposeSeqs xs) (* Correct Trans implementation *)
    | apply (Const x) obj = obj (* Replace this stub. *)
    | apply (Sel i) obj = obj (* Replace this stub. *)
    | apply (Map f) obj = obj (* Replace this stub. *)
    | apply (Reduce f) obj = obj (* Replace this stub. *)
    | apply (BinaryToUnary(ff, x)) obj = obj (* Replace this stub. *)
    | apply (Compose(ff1,ff2)) obj = obj (* Replace this stub. *)
    | apply (FunSeq funs) obj = obj (* Replace this stub. *)
    | apply funform  obj = (* Keep this catch-all error case *)
      raise (EvalError ("Ill-formed application: apply "
                        ^ (funFormToString funform)
			^ " " 
                        ^ (objToString obj)))

  (* testApply: funForm -> obj -> string *)
  and testApply funForm obj =
      objToString (apply funForm obj)
      handle (EvalError msg) => "EvalError: " ^ msg
           | (SyntaxError msg) => "SyntaxError: " ^ msg
           | other => "Exception: " ^ (exnMessage other)

  (* transposeSeqs: obj list -> obj list. Helper function for transpose *)
  and transposeSeqs seqs =
      let
	  (* Transpose a nonempty list of lists of the same length. *)	       
	  fun transposeLOL ([] :: _) = []
	    | transposeLOL lol = (List.map List.hd lol)
				 ::(transposeLOL (List.map List.tl lol))

	  (* Predicate that returns true only for sequences. *)	       
	  and isSeq (Seq _) = true
	    | isSeq _ = false

	  (* Return the number of elements in a sequence object. *)
	  and seqLen (Seq xs) = length xs
	    | seqLen  _ = raise (EvalError "seqLen: not a sequence")

	  and allSame [] = true
	    | allSame (lst as fst::rst) = List.all op= (ListPair.zip(lst,rst))
						   
	  and seqElts (Seq xs) = xs
	    | seqElts _ = raise (EvalError "not a sequence")

      in 				
	  if not (List.all isSeq seqs) then 
	      raise (EvalError ("transpose -- not a sequence of sequences: "
				^ (objToString (Seq seqs))))
	  else if not (allSame (List.map seqLen seqs)) then
	      raise (EvalError ("transpose -- sequences not all of same length: "
				^ (objToString (Seq seqs))))
	  else
	      List.map Seq (transposeLOL (List.map seqElts seqs))
      end

  (************************************************************
   Put any helper functions you define here. 
   ************************************************************)		  
		   
end (* end structure MiniFPInterp *)

open MiniFPInterp; (* open module by default, so don't have to open it manually *)
(* increase default printDepth, printLength, stringDepth *)

(* Increase default printing controls *)
val _ = Control.Print.printDepth := 10000;
val _ = Control.Print.printLength := 10000;
val _ = Control.Print.stringDepth := 10000;

