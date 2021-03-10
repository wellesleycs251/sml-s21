(*********************************************************************
 fib exercise
*********************************************************************)

fun fib n =
    if n < 2 then
	n
    else
	fib (n-1) + fib (n-2)

(*********************************************************************
 Racket to SML translation exercise
 *********************************************************************)

fun sumBetween lo hi = (* camelCase recommended over snake_case *)
    if lo > hi then 
	0
    else
	lo + sumBetween (lo+1) hi

(* 
   (* The following alternative definition does *not* work,
      because sumBetweenAlt in the body is *not* in the scope
      of the val declaration. *)

val sumBetweenAlt = fn lo => fn hi => 
    if lo > hi
    then 0
    else lo + sumBetweenAlt (lo+1) hi
*)

val v1 = sumBetween 3 7

val app_3_5 f = f 3 5

(* The following alternative definition *does* work,
   and has the same type as app_3_5 *)
val app_3_5_alt = fn f => f 3 5

val v2 = app_3_5 sumBetween

fun makeLinear a b x = a*x + b		 

(* The following alternative definitions *do* work,
   and have the same type as makeLinear *)
fun makeLinearAlt1 a b = fn x => a*x + b
fun makeLinearAlt2 a = fn b => fn x => a*x + b
val makeLinearAlt3 = fn a => fn b => fn x => a*x + b

val v3 = app_3_5 makeLinear 10

fun addPairs (a,b) (c,d) = (a+c, b+d)		 

(* The following alternative definitions *do* work,
   and have the same type as addPairs *)		 
fun addPairs2 (a,b) = fn (c,d) => (a+c, b+d)
val addPairs3 = fn (a,b) => fn (c,d) => (a+c, b+d)

fun addPairsLet p1 p2 =
    (* let can be used to desconstruct tuples via pattern matching *)
    let val (a,b) = p1
	val (c,d) = p2
    in (a+c, b+d)
    end

val v4 = addPairs (1,5) (3,4)

(*********************************************************************
 iterative Fibonacci exercise
 *********************************************************************)

fun fib_iter n = fib_tail n 0 0 1

(* Using `and` in place of `fun` is critical if `fib_tail`
   is defined *after* fib_iter, due to scoping issues in SML.
   `fun`/`and` defines mutually recursive funcions in the *same*
   scope, whereas `fun`/`fun` defines two (potentially individually
   recursive functions) one after the other *)
and fib_tail n i fib_i fib_i_plus_1 =
    if i = n then
	fib_i
    else
	fib_tail n (i+1) fib_i_plus_1 (fib_i + fib_i_plus_1)

(* It's good practice to define the tail-recursive function 
   *inside* the wrapper function. In addition to "hiding" the
   tail-recursive function, it can often take fewer parameters
   because it can use parameter names (like `n` in this case)
   from the enclosing scope. *)
fun fib_iter_nested n =
  let fun fib_tail i fib_i fib_i_plus_1 =
	if i = n then
	    fib_i
	else
	    fib_tail (i+1) fib_i_plus_1 (fib_i + fib_i_plus_1)
  in fib_tail 0 0 1
  end
