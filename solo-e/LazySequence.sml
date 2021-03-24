use "../utils/Utils.sml"; (* For Utils.range *)

(* Put your name here:    *)

(* CS 251 Fall '20 Solo Assignment E Problem 1 LazySequence *)

signature SEQUENCE = sig

    (* The type of a sequence *)
    type ''a t 

    (* An empty sequence *)
    val empty : ''a t

    (* Given integers lo and hi and an (int -> ''a) function fcn,
       if hi >= lo, returns a sequence of (hi - lo) values fcn(lo), fcn(lo+1), ..., fcn(hi-1);
       if hi < lo, returns an empty sequence. *)
    val segment: int -> int -> (int -> ''a) -> ''a t

    (* Convert a length-n list into a sequence of n values *)				
    val fromList: ''a list -> ''a t

    (* Concatenate two sequences: given	a length-m sequence s and length-n sequence t,
       returns a single length-m+n sequence that has all values of s followed by all
       values of t *)
    val concat : ''a t -> ''a t -> ''a t

    (* Return the length of a sequence = number of values in it *)				
    val length : ''a t -> int

    (* Return the nth value of a sequence (0-indexed).
       Raise an IndexOutOfBounds exception for an out-of-bounds index. *)
    val get : int -> ''a t -> ''a

    (* Return a sequence that results from applying f to each elt *)
    val map : (''a -> ''b) -> ''a t -> ''b t

    (* Return a list with all elements in the sequence. The ith element of the resulting
       list should be the ith element of the given sequence. *)					   
    val toList : ''a t -> ''a list

end

exception Unimplemented (* Placeholder during development. *)
exception IndexOutOfBounds of int (* when use nth with out-of-bounds index *)

structure LazySequence :> SEQUENCE = struct

    type ''a thunkTy = unit -> ''a

    datatype ''a t = Segment of int * int * (int -> ''a) (* lo, hi, fcn *)
		   | ThunkList of ''a thunkTy list
		   | Concat of ''a t * ''a t

    fun segment lo hi fcn = if hi >= lo 
			    then Segment(lo,hi,fcn) 
			    else Segment(lo, lo, fcn) (* an empty segment if hi < lo *)
    fun fromList xs = ThunkList (List.map (fn x => (fn () => x)) xs)
    fun concat seq1 seq2 = Concat(seq1,seq2)
    fun dethunk thunk = thunk()				   

    val empty = ThunkList []

    fun length seq = 0 (* replace this stub *)

    fun get n seq = raise Unimplemented (* replace this stub *)
	    
    fun map f seq = empty (* replace this stub *)

    fun toList seq = [] (* replace this stub *)

end

(* Test this code by using LazySequenceTest.sml *)
