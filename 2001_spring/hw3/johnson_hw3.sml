type (''k,'v) assoc_list = (''k * 'v) list
type Records = {losses:int, wins:int}
		   
exception NotFound
exception NotInLeague
exception WinnerLoserSame
val empty_assoc_list: (''k,'v) assoc_list = []
val init_rec = {losses= 0, wins=0}
		   
fun store ([],k,v) = [(k,v)]
  | store ((x as (k1,_))::xs',k,v) = if k1 = k then (k,v)::xs' else x::store(xs',k,v)
										       
fun fetch ([],k) = raise NotFound
  | fetch ((k1,v)::xs',k) = if k1 = k then v else fetch(xs',k)
						       
fun create_league ([]) = []
  | create_league (x::xs') = store((create_league xs'), x, init_rec)
  
fun record_game ([], _) = raise NotInLeague
  | record_game (ls, {loser,winner}) =
    let 
	val {losses=ll,wins=lw} = fetch(ls, loser) handle NotFound => raise NotInLeague
	val update_loser = store (ls,loser, {losses=ll+1,wins=lw})
	val {losses=wl,wins=ww} = fetch(update_loser, winner) handle NotFound => raise NotInLeague
	val updated_list as update_winner = store (update_loser, winner, {losses=wl,wins=ww+1})
    in updated_list
    end

fun better_record ({losses=l1,wins=w1},{losses=l2,wins=w2}) = (w1-l1)>(w2-l2)
fun sort_standings [] = []
  | sort_standings ((r as (_,x as pivot))::xs') = 
    sort_standings((List.foldl (fn(e,acc)=> if better_record(x,#2 e) then acc@[e] else acc) [] xs'))@
    (List.foldl (fn(e,acc)=> if (not (better_record(x,#2 e))) andalso
				(not (better_record(#2 e,x)))
			     then acc@[e]
			     else acc)
		[r]
		xs')@
    sort_standings((List.foldl (fn(e,acc)=> if better_record(#2 e,x) then acc@[e] else acc) [] xs'))

		  
fun print_league l =
  List.foldl (fn( (n,{losses=l,wins=w}),acc)=> 
		 (print ("\n" ^ Int.toString(acc) ^ ") "^n^" Losses: "^(Int.toString(l))^" | Wins: "^(Int.toString(w))); acc + 1))
	     1
	     (sort_standings l)

