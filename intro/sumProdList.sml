fun sumProdList [] = (0, 1)
  | sumProdList (n::ns) =
    let val (sum, prod) = sumProdList ns
    in (n+sum, n*prod)
    end

fun sumProdListFoldr ns = 
    foldr (fn (n, (sum, prod)) => (n+sum, n*prod))
	  (0, 1)
	  ns
