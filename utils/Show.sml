(* From https://stackoverflow.com/questions/22150715/convert-datatype-to-string-sml *)

structure Show =
   struct
      (* Show.t is the type of toString functions *)
      type 'a t = 'a -> string

      val int: int t = Int.toString

      val bool: bool t = Bool.toString

      val char: char t = Char.toString

      val string: string t = fn str => str

      (* Lyn added this *)
      val quotedString: string t = fn str => "\"" ^ str ^ "\""

      val fcn: ('a -> 'b) t = fn f => "<fn>"

      val list: 'a t -> 'a list t =
       fn showElt => fn elts => "[" ^ (String.concatWith "," (List.map showElt elts)) ^ "]"

      val pair: 'a t * 'b t -> ('a * 'b) t =
       fn (showa,showb) => fn (a,b) => "(" ^ showa a ^ "," ^ showb b ^ ")"

      val triple: 'a t * 'b t * 'c t -> ('a * 'b * 'c) t =
       fn (showa,showb,showc) => fn (a,b,c) => 
				    "(" ^ showa a ^ "," ^ showb b ^ "," ^ showc c ^ ")"

      (* ... *)
   end
