(* This is the contents of the file step.sml *)

fun stepUntil ((a,b), limit) = (* no looping constructs in ML; *)
  if a >= limit then           (* use tail recursion instead! *)
      (a,b)
  else
      stepUntil (step(a,b), limit)

fun step (a,b) = (a+b, a*b)		
