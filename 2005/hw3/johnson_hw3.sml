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

fun lineprog_func prog pair = List.foldl (fn(f,acc) => f acc) pair (List.map lineop_func prog);
      (*
fun lineprog_func prog pair =
  case prog of
      x::[] => pair
    | x::xs' => lineprog_func xs' (lineop_func x pair)

val fl1 =  (List.foldl (fn(f,acc) => f acc)) o (List.map lineop_func p);
*)

fun pushn_right_safe n =
  case (n, n > 0) of
      (_,false) => []
    | (1,true) => [PushRight, ApplyRight(fn(x,y)=>(y,x))]
    | (n,true) =>  PushRight::ApplyRight(fn(x,y)=>(y,x))::(pushn_right_safe (n-1))
								     
fun pushn_right_block n =
  let fun push_right m =
	case (m, m > 0) of
	    (_, false) => []
	  | (m, true) => PushRight::(push_right (m-1))
      fun swap_left m =
	case (m, m > 0) of
	    (_, false) => [SwapFirst]
	  | (m, true) => SwapFirst::PushLeft::(swap_left (m-1))
  in case (n, n < 2,n < 1) of
	 (_,_,true) => []
       | (1,_,false) => [PushRight]
       | (_,false,_) => push_right(n-1)@swap_left(n-2)@pushn_right_block(n-1)
  end

fun lineprog_mirror prog = List.map(fn(e)=> case e of
						PushRight => PushLeft
					      | PushLeft => PushRight
					      | ApplyRight f => ApplyLeft f
					      | ApplyLeft f => ApplyRight f
					      | _ => e) prog

val pushn_left_safe = lineprog_mirror o pushn_right_safe
val pushn_left_block = lineprog_mirror o pushn_right_block
					     
fun lineprog_optimize prog =
  case prog of
      [] => []
    | x::[] => [x]
    | PushRight::PushLeft::xs'' => lineprog_optimize xs''
    | PushLeft::PushRight::xs'' => lineprog_optimize xs''
    | SwapFirst::SwapFirst::xs'' => lineprog_optimize xs''
    | x::xs'::xs'' => x::xs'::(lineprog_optimize xs'')

val bignumber = 100000.0
val smallnumber = 1.0/bignumber
fun derivative f x = ((f (x + smallnumber)) - (f x))/smallnumber

fun def_integral (f:real->real) a b =
  if (a + smallnumber) < b
  then (f a) + (def_integral f (a+smallnumber) b)
  else f a
	 
fun integral f = def_integral f 0.0
			      
