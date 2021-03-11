(* Add three new figure constructors: 			     
     1. Sq for a square with a side length
     2. FigPair for a group of exactly two figures
     3. FigGroup for a group of a list of any number of figures

   The perimeter of FigPair and FigGroup is the sum of the perimeters of the
   the figures in the group. Use Real.+ to add reals and 0.0 for real zero. 

   Scaling a FigPair or FigGroup by n scales each figure in the group by n.

   See examples in the comments below. 

 *)

(* Set printLength & printDepth appropriately *)		     
Control.Print.printLength := 100;
Control.Print.printDepth := 100;

datatype figure =
    Circ of real (* radius *)
  | Rect of real * real (* width, height *)
  | Tri of real * real * real (* side1, side2, side3 *)
  | Sq of real (* side *)
  | FigPair of figure * figure
  | FigGroup of figure list

val c = Circ 1.0
val r = Rect (2.0,3.0)
val t = Tri(4.0,5.0,6.0)
val s = Sq(7.0)
val p = FigPair(r, s)
val g = FigGroup([c, t, p])
val figs = [c, r, t, s, p, g] (* List of sample figures *)

(* 	       
  (* Lyn has manually reformatted this: *)
  val figs = [Circ 1.0,Rect (2.0,3.0),Tri (4.0,5.0,6.0),Sq 7.0,
              FigPair (Rect (2.0,3.0),Sq 7.0),
              FigGroup [Circ 1.0,
                        Tri (4.0,5.0,6.0),
                        FigPair (Rect (2.0,3.0),Sq 7.0)]]
  : figure list
 *) 

fun perim (Circ r) = 2.0 * Math.pi * r
  | perim (Rect(w,h)) = 2.0 * (w + h)
  | perim (Tri(s1,s2,s3)) = s1 + s2 + s2
  | perim (Sq(s)) = s*s
  | perim (FigPair(f1,f2)) = perim(f1) + perim(f2)
  | perim (FigGroup(fs)) = foldr Real.+ 0.0 (map perim fs)

val perims = map perim figs

(* 
  val perims = [6.28318530718,10.0,14.0,49.0,59.0,79.2831853072] : real list
 *)		 

fun scale n (Circ r) = Circ (n * r) (* Scale figure by factor n *)
  | scale n (Rect(w,h)) = Rect (n*w, n*h)
  | scale n (Tri(s1,s2,s3)) = Tri (n*s1, n*s2, n*s3)
  | scale n (Sq(s)) = Sq(n*s)
  | scale n (FigPair(f1,f2)) = FigPair(scale n f1, scale n f2)
  | scale n (FigGroup(fs)) = FigGroup(map (fn f => scale n f) fs)

val scaledFigs = map (scale 3.0) figs;

(* 
  val scaledFigs =
    [Circ 3.0,Rect (6.0,9.0),Tri (12.0,15.0,18.0),Sq 21.0,
     FigPair (Rect (6.0,9.0),Sq 21.0),
     FigGroup [Circ 3.0,Tri (12.0,15.0,18.0),FigPair (Rect (6.0,9.0),Sq 21.0)]]
    : figure list
 *)		 

