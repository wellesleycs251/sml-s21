fun sumListRec [] = 0
  | sumListRec (n :: ns) = n + (sumListRec ns)

fun sumListIter nums = sumListTail nums 0
				   
and sumListTail [] sumSoFar = sumSoFar
  | sumListTail (n :: ns) sumSoFar =
    sumListTail ns (n + sumSoFar)
      
