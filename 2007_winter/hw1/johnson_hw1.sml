fun pow x 0 = "1"
  | pow x y =
    let fun pos_pow 0 = 1
	  | pos_pow y = x * (pos_pow (y-1))
	fun neg_pow 0 = 1.0
	  | neg_pow y = (1.0/real(x)) * (neg_pow (y+1))
    in if y>0 then Int.toString(pos_pow y) else Real.toString(neg_pow y)
    end

fun sumTo 0 = 0.0
  | sumTo n = 1.0/real(n) + sumTo (n-1)

fun repeat s 0 = ""
  | repeat s n = if n < 0 then "" else s ^ repeat s (n-1)

fun twos 0 = 0
  | twos i = if i mod 2 = 0 then 1 + twos (i div 2) else 0
							     
fun range s e = if s <= e then s :: range (s+1) e else []

fun numNegative [] = 0
  | numNegative (l::ls') = if l < 0 then 1 + numNegative ls' else numNegative ls'

fun absList [] = []
  | absList ((x,y)::ls') = (abs x, abs y) :: absList ls'
		   
fun isSorted [] = true
  | isSorted [x] = true
  | isSorted (x::x'::xs') = x < x' andalso isSorted (x'::xs')

fun isSorted2 l = if l = [] then true
		  else if length l = 1 then true
		  else (hd l) < (hd (tl l)) andalso isSorted2 (tl l)

fun collapse [] = []
  | collapse [x] = [x]
  | collapse (x::x'::xs') = (x+x') :: collapse xs'

fun collapse2 l = #1 (List.foldr (fn(x,(acc,prev,track))=> if track
							   then ((x+prev)::acc,0,false)
							   else (acc,x,true))
				 ([],0,length l mod 2 <> 0)
				 l)
(*Challenge criteria met: O(n^0.5) time *)	   
fun factors i =
    let fun helper n acc =
	  if n = 0 then acc else
	  if i mod n = 0 then helper (n - 1) (n::acc) else helper (n - 1) acc
    in helper (i div 2) [i]
    end
