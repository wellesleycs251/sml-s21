(* This is the contents of the file
     /home/u/cs251/sml/intro/mydefns.sml on the csenv appliance.
   It can also be found on cs.wellesley.edu in 
     ~cs251/download/sml/intro/mydefns.sml 
  It defines integers a and b and functions named sq, sos, and fact.
  (* By the way, comments nest properly in SML! *)   
*)

val a = 2 + 3
val b = 2 * a

fun sq n = n * n (* squaring function *)

fun sos a b = sq a + sq b (* sum-of-squares function *)

(* calculate hypotenuse of right triangle with sides a and b *)
fun hyp a b = Math.sqrt(Real.fromInt(sos a b))

fun fact n = (* a recursive factorial function *)
  if n = 0 then
    1
  else
    n * (fact (n - 1)) 

