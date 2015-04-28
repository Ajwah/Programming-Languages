(*Johnson homework1*)
val max = 7
fun legal_bone(s1,s2) = s1 >= 1 andalso s1 <= max andalso s2 >= 1 andalso s2 <= max
fun compatible_bones(b1: int*int,b2: int*int) = (#1 b1) = (#1 b2) orelse (#1 b1) = (#2 b2) orelse (#2 b1) = (#1 b2) orelse (#2 b1) = (#2 b2)
																	 
fun all_legal_bones(blist) =
  case blist of
      [] => true
    | (x::xs') => legal_bone(x) andalso all_legal_bones(xs')
						       
fun no_doubles(blist) =
  case blist of
      [] => true
    | ((s1,s2)::xs') => s1 <> s2 andalso no_doubles(xs')
						   
fun make_suit(s) =
  let fun helper(n) =
	if n >= 1
	then if n = s
	     then (s,n)::helper(n-1)
	     else (n,s)::(s,n)::helper(n-1)
	else []
  in helper(s)
  end
      
fun all_same_suit(blist) =
  let fun helper(s,ls) =
	case ls of
	    [] => true
	  | (s1,s2)::xs' => (s1 = s orelse s2 = s) andalso helper(s, xs')
  in case blist of
	 [] => false
       | x::[] => true
       | (s1,s2)::xs' => helper(s1, xs') orelse helper(s2, xs')
  end
      
