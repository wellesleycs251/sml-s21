structure Shuffle = struct

fun nshuffle n lst =
   if n = 0 then
      lst
   else
      nshuffle (n-1) (shuffle lst)

and shuffle lst =
    if (length lst) mod 2 = 0 then
	shuffleEven lst
    else
        (List.hd lst) :: (shuffleEven (List.tl lst))

and shuffleEven [] = []
  | shuffleEven lst =
    let val half = (length lst) div 2
    in List.concat(map (fn (a,b) => [a,b])
                       (ListPair.zip (List.take(lst, half),
                                      List.drop(lst, half))))
    end

(* number of perfect shuffles to get back to original *)
and numShuffles n = 
    let val orig = Utils.range 0 n 
	fun loop lst count = 
	    if lst = orig then
		count
	    else
		loop (shuffle lst) (count + 1)
    in loop (shuffle orig) 1
    end

and allShuffles n = 
    let val orig = Utils.range 0 n 
	fun loop lst shuffles = 
	    if lst = orig then
		lst :: shuffles
	    else
		loop (shuffle lst) (lst :: shuffles)
    in loop (shuffle orig) [] 
    end

end
