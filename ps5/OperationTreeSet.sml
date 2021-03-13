(* Put your name(s) here:    *)

(* CS 251 Spring '21 Problem Set 5 Problem 4 OperationTreeSet. 

   Complete the operation-tree representation of the SET signature.
   Represent a set as a tree of set operations. 

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

    (* Construct a set from a list of elements.
       Do not assume the list elements are unique. *)
    val fromList : ''a list -> ''a t 

    (* Convert a set to a list without duplicates. 
       The elements in the resulting list may be in any order. *)
    val toList : ''a t -> ''a list

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

(* Implement a SET ADT using membership predicates to represent sets. *)

structure OperationTreeSet :> SET = struct

    datatype ''a t = Empty
		   | Insert of ''a * ''a t
		   | Delete of ''a * ''a t
		   | Union of ''a t * ''a t
		   | Intersection of ''a t * ''a t
		   | Difference of ''a t * ''a t
					       
    val empty = Empty
    fun insert x s = Insert(x,s)
    fun singleton x = Insert(x, Empty)
    fun delete x s = Delete(x, s)
    fun union s1 s2 = Union(s1,s2)
    fun intersection s1 s2 = Intersection(s1,s2)
    fun difference s1 s2 = Difference(s1,s2)

    fun member _ _ = raise Unimplemented  
    fun toList _ = raise Unimplemented
    fun isEmpty _ = raise Unimplemented
    fun size _  = raise Unimplemented
    fun toPred _ = raise Unimplemented
    fun toString eltToString _ = raise Unimplemented
    fun fromList _ = raise Unimplemented

    fun fromPred _ = raise Unimplementable

end

