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
			    then acc @ xs'
			    else helper(xs',(s1,s2)::acc)
  in helper(h,[])
  end
	     
