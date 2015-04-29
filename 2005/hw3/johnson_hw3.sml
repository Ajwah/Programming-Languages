(*JOHNSON hw3*)
type bone = int * int
type line = bone list
datatype lineop =
         PushRight (*Take element from left, push it onto right *)
         | PushLeft (*Take element from right, push it onto left*)
         | SwapFirst (* Take first bone from each line and swap them*)
         | ApplyRight of bone -> bone (*Apply function to first bone on the right*)
         | ApplyLeft of bone -> bone (*Apply function to first bone on the left*)

type lineprog = lineop list

(*
 val lineop_func = fn : lineop -> bone list * bone list -> bone list * bone list
     val lineprog_func = fn
       : lineop list -> bone list * bone list -> bone list * bone list
     val pushn_right_safe = fn : int -> lineop list
     val pushn_right_block = fn : int -> lineop list
     val lineprog_mirror = fn : lineop list -> lineop list
     val pushn_left_safe = fn : int -> lineop list
     val pushn_left_block = fn : int -> lineop list
     val derivative = fn : (real -> real) -> real -> real
val def_integral = fn : (real -> real) -> real -> real -> real
     val integral = fn : (real -> real) -> real -> real
*)
		       
fun lineop_func lop bpair =
  let fun helper (left,right) = 
      case (lop,left,right) of
	   (PushRight,[],_) => raise Empty
	 | (PushLeft,_,[])  => raise Empty
	 | (SwapFirst,[],_) => raise Empty
	 | (SwapFirst,_,[]) => raise Empty
	 | (ApplyRight f, _, []) => raise Empty
	 | (ApplyLeft f, [],_)  => raise Empty
	 | (PushRight,x::xs',right) => (xs',x::right)
	 | (PushLeft,left,x::xs') => (x::left,xs')
	 | (SwapFirst,x::xs',y::ys') => (y::xs',x::ys')
	 | (ApplyRight f, left, x::xs') => (left,f(x)::xs')
	 | (ApplyLeft f, y::ys',right)  => (f(y)::ys',right)
  in helper bpair
  end
