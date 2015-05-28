Control.Print.printDepth := 99; (* deep structs for debugging. yes, := *)
infix mem
fun x mem [] = false
  | x mem (y::ys) = x=y orelse x mem ys
fun addmem x xs = if (x mem xs) then xs else x::xs
fun remmem x xs = List.foldl (fn(y,acc)=> if y=x then acc else y::acc) [] xs
fun setof xs = List.foldl (fn(x,acc)=>addmem x acc) [] xs
infix union
fun xs union ys = List.foldl (fn(x,acc)=> addmem x acc) (setof ys) xs
fun isect_1 [] ys = []
  | isect_1 xs [] = []
  | isect_1 (x::xs') ys = if x mem ys then x :: isect_1 xs' ys else isect_1 xs' ys

fun isect2 xs ys = 
let fun helper [] ys acc = acc
      | helper xs [] acc = acc
      | helper (x::xs') ys = if x mem ys then helper xs' ys (x::acc) else helper xs' ys acc

fun isect_foldl xs ys =  List.foldl (fn(x,acc) if x mem ys then x::acc else acc) [] xs
fun isect_filter xs ys = List.filter (fn(x)=> x mem ys) xs
				     
			     
