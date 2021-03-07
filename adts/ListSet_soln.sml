(* CS 251 Set ADT exercises.
   Complete the list-based representation of the SET signature.
   Represent a set as an unordered list of elements without duplicates. 

   You may ignore "Warning: calling polyEqual" in this exercise. *)

exception Unimplemented (* Placeholder during development. *)
exception Unimplementable (* Impossible to implement *)

(* SET describes operations over set values of type ''a t, where set
   elements are of type ''a.  The double-quote type variable ''a means 
   that values of the type ''a can be compared using the = operation.

   Naming the type of the ADT t is a common idiom for signatures
   defining an ADT.  This means that for particular implementations
   (e.g., ListSet or FunSet), ADT values have type ListSet.t or
   FunSet.t, rather than the more verbose ListSet.set or FunSet.set.
   If a signature defines multiple types (especially if there's not
   one main type and other supporting types), this idiom is less
   commonly used. *)

signature SET =
sig
    (* The type of sets *)
    type ''a t 

    (* An empty set *)
    val empty : ''a t

    (* Construct a single-element set from that element. *)
    val singleton : ''a -> ''a t

    (* Check if a set is empty. *)
    val isEmpty : ''a t -> bool

    (* Return the number of elements in the set. *)
    val size : ''a t -> int

    (* Check if a given element is a member of the given set. *)
    val member : ''a -> ''a t -> bool

    (* Construct a set containing the given element and all elements
       of the given set. *)
    val insert : ''a -> ''a t -> ''a t

    (* Construct a set containing all elements of the given set
       except for the given element. *)
    val delete : ''a -> ''a t -> ''a t

    (* Construct the union of two sets. *)
    val union : ''a t -> ''a t -> ''a t

    (* Construct the intersection of two sets. *)
    val intersection : ''a t -> ''a t -> ''a t

    (* Construct the difference of two sets
       (all elements in the first set but not in the second.) *)
    val difference : ''a t -> ''a t -> ''a t

    (* Construct a set from a list of elements.
       Do not assume the list elements are unique. *)
    val fromList : ''a list -> ''a t 

    (* Convert a set to a list without duplicates. 
       The elements in the resulting list may be in any order. *)
    val toList : ''a t -> ''a list

    (* Construct a set from a predicate function:
       the resulting set should contain all elements for which
       this predicate function returns true.

       This acts like math notation for sets.  For example:
         { x | x mod 3 = 0 }
       would be written:
         fromPred (fn x => x mod 3 = 0)
    *)
    val fromPred : (''a -> bool) -> ''a t

    (* Convert a set to a predicate function. *)
    val toPred : ''a t -> ''a -> bool

    (* Convert a set to a string representation, given a function
       that converts a set element into a string representation. *)
    val toString : (''a -> string) -> ''a t -> string

end

(* Implement a SET ADT using lists to represent sets. *)
structure ListSet :> SET = struct

    (* Invariant: the list does not contain duplicates. No ordering is implied *)

    (* Sets are represented by lists. *)
    type ''a t = ''a list

    (* The empty set is the empty list. *)
    val empty = []

    (* complete this structure by replacing "raise Unimplemented"
       with implementations of each function *)
    fun singleton x = [x]

    fun isEmpty xs = null xs (* or x = [] *)
			  
    fun size xs = length xs
			 
    fun member x ys = List.exists (fn y => x = y) ys

    fun insert x ys = if member x ys then ys else x :: ys			

    fun delete x ys = List.filter (fn y => x <> y) ys

    fun union xs ys = 
	(* One strategy for union is to start with the foldr from fromList, 
           and adapt it to use ys as nullval and insert all xs *)
	foldr (fn (x,setSoFar) => insert x setSoFar) ys xs

        (* Another strategy for union is to append to xs all values
           from ys that are not in xs:

               xs @ (List.filter (fn y => not (member y xs)) ys)

         *)
 
    fun intersection xs ys = List.filter (fn x => member x ys) xs
				   
    fun difference xs ys = List.filter (fn x => not (member x ys)) xs				  

    fun fromList xs = foldr (fn (x, set) => insert x set) empty xs
	(* Can use repeated insert via foldr or foldl to remove duplicates.
           foldr will insert the elements back to front while 
           foldl will insert the elements from front to back, 
           so element order may differ (but it's irrelevant). 

           Note: Can't just use insert as combiner because insert takes
                 curried args, and combiner must have tupled args. 
                 Alternatively, we could define uncurry2 as

                     fun uncurry2 curriedFun (x,y) = curriedFun x y

                 and then use

                     foldr (uncurry2 insert) empty xs
  
          *)

    fun toList xs = xs

    fun fromPred _ = raise Unimplementable (* impossible to implement! *)
			   
    fun toPred xs = fn x => member x xs
			 
    fun toString eltToString xs = 
	"{" ^ (String.concatWith "," (map eltToString xs)) ^ "}"
			     
end
                               
(* Some tests cases. Can add more of your own *)

open ListSet;

val s1 = insert 1 (insert 2 (insert 3 empty)) (* {1,2,3} *)

val s2 = insert 2 (insert 4 (insert 1 s1)) (* {1,2,3,4} *)

val s3 = fromList [3,4,5,6,7]; (* {3,4,5,6,7} *)

val delSet = delete 8 (delete 6 (delete 4 s3))

val unionSet = union s2 s3

val isectSet = intersection s2 s3

val diffSet = difference s3 s2

val namedSets = [("empty", empty), ("s1", s1), ("s2", s2), ("s3", s3),
		 ("delSet", delSet), ("unionSet", unionSet), 
		 ("isectSect", isectSet), ("diffSet", diffSet)]

fun testSetFunction setFun = map (fn (name,set) => (name, setFun set)) 
				 namedSets

val toListTests = testSetFunction toList

val sizeTests = testSetFunction size

val isEmptyTests = testSetFunction isEmpty

val toStringTests = testSetFunction (toString Int.toString)

fun testMember set = map (fn i => (i, member i set))
			 [0,1,2,3,4,5,6,7,8]

val memberTests = testSetFunction testMember

fun testToPred set = let val pred = toPred set 
		     in map (fn i => (i, pred i))
			    [0,1,2,3,4,5,6,7,8]
		     end

val toPredTests = testSetFunction testToPred


