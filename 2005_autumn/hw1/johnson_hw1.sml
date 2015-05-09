fun spherevolume r = 4.0/3.0*Math.pi*Math.pow(r,3.0)					
fun squares [] = []
  | squares (x::xs') = (x*x)::(squares xs')
val squares2 = List.map (fn(x)=> x*x)
fun repeat r n = if n <= 0 then [] else r::(repeat r (n-1))
fun repeatlist [] _ = []
  | repeatlist (x::xs') n =
    let fun loop 0 = repeatlist xs' n
	  | loop i = x::(loop (i-1))
    in loop n
    end
fun ascending [] = false
  | ascending [x] = false
  | ascending ls =
    let fun eval [x] = true
	  | eval (x::x'::xs') = x<x' andalso eval (x'::xs')
    in eval ls
    end
	
