fun incList [] = []
  | incList (n :: ns) = n+1 :: incList ns

fun filterPos [] = []
  | filterPos (n :: ns) =
    if n > 0 then
	n :: (filterPos ns)
    else
	filterPos ns

fun myFilter pred [] = []
  | myFilter pred (x :: xs) =
    if pred x then
	x :: (myFilter pred xs)
    else
	myFilter pred xs
