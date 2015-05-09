fun spherevolume r = 4.0/3.0*Math.pi*Math.pow(r,3.0)					
val squares = List.map (fn(x)=> x*x)
fun repeat r n = if n <= 0 then [] else r::(repeat r (n-1))
fun repeatlist ls n = List.foldl (fn(x,acc)=> (repeat x n)@acc) [] ls
fun ascending [] = false
  | ascending [x] = false
  | ascending (x::xs') = #2 (List.foldl (fn(y,(prev,res))=> (y,y>prev andalso res)) (x,true) xs')

(* Previous implementations of above functions *)
fun ascending_long [] = false
  | ascending_long [x] = false
  | ascending_long ls =
    let fun eval [x] = true
	  | eval (x::x'::xs') = x<x' andalso eval (x'::xs')
    in eval ls
    end
	
fun squares_long [] = []
  | squares_long (x::xs') = (x*x)::(squares xs')
fun repeatlist_long [] _ = []
  | repeatlist_long (x::xs') n =
    let fun loop 0 = repeatlist_long xs' n
	  | loop i = x::(loop (i-1))
    in loop n
    end
