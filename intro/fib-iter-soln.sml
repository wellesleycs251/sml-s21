fun fib_iter n =
  let fun fib_tail i fib_i fib_i_plus_1 =
	if i = n then
	    fib_i
	else
	    fib_tail (i+1) fib_i_plus_1 (fib_i + fib_i_plus_1)
  in fib_tail 0 0 1
  end





