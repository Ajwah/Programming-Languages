(*
Compute the length of a list (without using the built in length function.)
Cycle a list once. That is, given the list [1, 2, 3, 4, 5], produce the output [2, 3, 4, 5, 1].
Cycle a list the other way. That is, given the list [1, 2, 3, 4, 5], produce the output [5, 1, 2, 3, 4].
Remove all of the odd numbers from a list of integers.
Remove all of the vowels from a string.
*)
fun compute_length [] = 0
  | compute_length (_::xs') = 1 + compute_length xs'

fun cyclel (x::xs') = xs'@[x]
  | cyclel [] = []
		    
fun cycler l =
  let fun helper l acc =
	case l of
	    [] => []
	  | x::[] => x::acc
	  | x::xs' => helper xs' (acc@[x])
  in helper l []
  end

fun remove_odds [] = []
  | remove_odds (x::xs') = case (x mod 2 = 0) of
			       true => x::(remove_odds xs')
			     | false => remove_odds xs'
						    
val remove_odds2 = List.foldl (fn(x,acc)=>if (x mod 2) = 0 then acc@[x] else acc) []
								 
fun remove_vowels l = String.implode(List.foldl (fn(x, acc)=>
						    if not (x = #"a" orelse
							    x = #"e" orelse
							    x = #"i" orelse
							    x = #"o" orelse
							    x = #"u" orelse
							    x = #"A" orelse
							    x = #"E" orelse
							    x = #"I" orelse
							    x = #"O" orelse
							    x = #"U"
							   )
						    then acc@[x]
						    else acc)
						[]
						(String.explode(l)));
				  
					
