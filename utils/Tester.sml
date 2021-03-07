structure Tester = struct 

fun curry2 f (x,y) = f x y
fun curry3 f (x,y,z) = f x y z
fun curry4 f (w,x,y,z) = f w x y z

fun testFn (fnName, argsToString, resultToString, studFn, solnFn, argsList) = 
    let val numPassed = ref 0
	val numFailed = ref 0
	fun testArgs (args) = 
	    let val _ = print ("\n" ^ fnName ^ " ")
		val _ = print (argsToString args)
		val _ = print ": "
		val studRes = studFn args 
		val solnRes = solnFn args
	    in if studRes = solnRes then 
		   (numPassed := 1 + !numPassed; 
		    print "passed")
	       else 
		   (numFailed := 1 + !numFailed; 
		    print "***FAILED";
		    print "\n Expected: "; 
		    print (resultToString solnRes);
		    print "\n   Actual: "; 
		    print (resultToString studRes))
	    end
            handle exn => 
	      (numFailed := 1 + !numFailed; 
	       print ("***FAILED DUE TO EXCEPTION in " ^ fnName ^ " " 
	              ^ (argsToString args) ^ ": "
		      ^ (exnName exn) ^ " " ^ (exnMessage exn)))
	val _ = print "\n--------------------------------------------------------"
	val _ = List.app testArgs argsList
	val numTests = !numPassed + !numFailed 
    in
	if !numFailed = 0 then
	    (print ("\nPassed all " ^ (Int.toString numTests) ^ " test cases\n"); 
	     (true, !numPassed, !numFailed))
	else
	    (print ("\nFailed " ^ (Int.toString (!numFailed)) ^ " of " 
		    ^ (Int.toString numTests) ^ " test cases\n"); 
	     (true, !numPassed, !numFailed))
    end

fun callTesters testerList = 
    let val numPassed = ref 0
	val numFailed = ref 0
	fun callTester(tester) = 
	    let val (_, passes, fails) = tester()
		val _ = numPassed := passes + !numPassed
		val _ = numFailed := fails + !numFailed
	    in ()
	    end
	val _ = List.app callTester testerList
	val numTests = !numPassed + !numFailed
	val _ = print "\n************************************************************"
    in
	if !numFailed = 0 then
	    (print ("\nSUMMARY: Passed all " ^ (Int.toString numTests) ^ " test cases\n"); 
	     (true, !numPassed, !numFailed))
	else
	    (print ("\nSUMMARY: Failed " ^ (Int.toString (!numFailed)) ^ " of " 
		    ^ (Int.toString numTests) ^ " test cases\n"); 
	     (true, !numPassed, !numFailed))
    end
				     
end (* structure *)
	 
	 


    
