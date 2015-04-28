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
			    else helper(xs',(s1,s2)::acc)
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
    | (l,h) => let val layout = layout_summary(l)
		   val left = #1 layout
		   val right = #2 layout
	       in case(find_playable(h,left),find_playable(h,right) of
			  (NONE,NONE) => PassDraw
			| (SOME b, _) => PlayLeft b
			| (_, SOME b) => PlayRight b
						   
		       end;

		       
				  
