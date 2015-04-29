type bone = int * int
		      
type hand = bone list
type deck = bone list
type layout = bone list

datatype move = PlayFirst of bone
              | PlayLeft of bone
              | PlayRight of bone
       | PassDraw

exception BadMove

fun find_playable(h,s) =
  case h of
      [] => NONE
    | (s1,s2)::xs' => if s1 = s orelse s2 = s
		      then SOME (s1,s2)
		      else find_playable(xs',s)
					
fun without_bone(h,b) =
  let fun helper(h,acc) =
	case h of
	    [] => acc
	  | (s1,s2)::xs' => if (s1,s2) = b orelse (s2,s1) = b
			    then List.concat(acc::xs'::[])
			    else helper(xs',List.concat(acc::[(s1,s2)]::[]))
  in helper(h,[])
  end

fun layout_summary(l) =
  let fun helper(l,pl) =
	case l of
	    (_,s2)::[] => SOME (pl,s2)
	  | _::xs' => helper(xs',pl)
  in case l of
	 [] => NONE
       | x::[] => SOME x
       | (s1,_)::xs' => helper(xs',s1)
  end

fun best_move(l,h) =
  case (l,h) of
      ([],[]) => PassDraw
    | ([],b::bs') => PlayFirst b
    | (l,h) =>
      let val SOME layout = layout_summary(l)
	  val left = (#1 layout)
	  val right = (#2 layout)
      in
	  case(find_playable(h,left),find_playable(h,right)) of
	      (NONE,NONE) => PassDraw
	    | (SOME b, _) => PlayLeft b
	    | (_, SOME b) => PlayRight b
      end
	  	       
fun do_move(l,d,h,m) =
  let fun helper(f, (s1,s2)) =
	case (List.find(fn(x) => x = (s1,s2) orelse x = (s2,s1)) h) of
	    NONE => raise BadMove
	 |  SOME b => f(b)
      fun get_left(l) = #1 (valOf (layout_summary(l)))
      fun get_right(l) = #2 (valOf (layout_summary(l)))
  in case (l,d,h,m) of
      (_,x::xs',h,PassDraw) => (l,xs',x::h)
    | (_,[],h,PassDraw) => (l,[],h)
    | (x::xs',_,_,PlayFirst b) => raise BadMove
    | ([],_,h,PlayFirst b) => helper((fn(rb) => ([rb],d,without_bone(h,rb))), b)
    | ([],_,_,PlayLeft _) => raise BadMove
    | ([],_,_,PlayRight _) => raise BadMove
    | (l,_,h,PlayLeft b) =>  helper((fn(s1,s2) => case (s1 = get_left(l), s2 = get_left(l)) of
						      (false,false) => raise BadMove
						    | (true,_) => ((s2,s1)::l,d,without_bone(h,(s1,s2)))
						    | (_,true) => ((s1,s2)::l,d,without_bone(h,(s1,s2)))), b)
    | (l,_,h,PlayRight b) => helper((fn(s1,s2) => case (s1 = get_right(l), s2 = get_right(l)) of
						      (false,false) => raise BadMove
						    | (true,_) => (l@[(s1,s2)],d,without_bone(h,(s1,s2)))
						    | (_,true) => (l@[(s2,s1)],d,without_bone(h,(s1,s2)))), b)
  end
							
