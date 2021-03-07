fun letTest () =
  let val a = 27 (* 1st let binding *)
      val b = 3 (* 2nd  binding *)
      fun fact x = x + 2 (* 3rd binding *)
  in fact (a div b) (* let body after in keyword *)
  end (* end terminates the let *)

fun fibIter n =
  let fun fibTail i fib_i fib_i_plus_1 = (* a local recursive function *)
	  if i = n then
	      fib_i
	  else
	      fibTail (i+1) fib_i_plus_1 (fib_i + fib_i_plus_1)
  in fibTail 0 0 1 (* call local recursive local function *)
  end

fun testEvenOdd n =
    let (* define two local mutually recursive functions *)
	fun isEven x = if x = 0 then true else isOdd(x-1)
	and isOdd y = if y = 0 then false else isEven(y-1)
    in (isEven n, isOdd n)
    end

val tup1 = (1+2, 3<4, 5*6, 7=8) (* Has value (3, true, 30, false) *)

val tup2 = ((#1 tup1) + (#3 tup1), (#2 tup1) orelse (#4 tup1))

val tup3 = let val (i1, b1, i2, b2) = tup1
	   in (i1 + i2, b1 orelse b2)
	   end







