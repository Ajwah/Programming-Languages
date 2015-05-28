Control.Print.printDepth := 99; (* deep structs for debugging. yes, := *)
infix mem
fun x mem [] = false
  | x mem (y::ys) = x=y orelse x mem ys
fun addmem x xs = if (x mem xs) then xs else x::xs
fun remmem x xs = List.foldl (fn(y,acc)=> if y=x then acc else y::acc) [] xs
			     
