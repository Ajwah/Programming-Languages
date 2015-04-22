(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, sl) =
  let fun helper(l) =
	case l of
	    [] => []
	  | x::xs' => if same_string(s,x) then helper(xs') else x::helper(xs')
  in
      case sl of
	  [] => NONE
        | _ => if helper(sl) = sl then NONE else SOME(helper(sl))
  end

fun get_substitutions1(sl,s) =
  case sl of
      [] => []
    | x::xs' => case all_except_option(s, x) of
		    NONE => get_substitutions1(xs',s)
		  | SOME l => l @ get_substitutions1(xs',s)

fun get_substitutions2(sl,s) =
  let fun helper(li, acc) =
	case li of
	    [] => acc
	  | x::xs' => case all_except_option(s,x) of
			  NONE => helper(xs',acc)
			| SOME l => helper(xs',l @ acc)
  in helper(sl,[])
  end

fun similar_names(sl,{first,middle,last}) =
  let fun helper(li) =
	case li of
	    [] => []
	  | x::xs' => {first=x,last=last, middle=middle}::helper(xs')
  in {first=first,last=last, middle=middle}::helper(get_substitutions1(sl, first))
  end
      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(suit,rank) =
  case suit of
      Clubs => Black
    | Spades => Black
    | _ => Red

fun card_value(suit,rank) =
  case rank of
      Ace => 11
    | Num x => x
    | _ => 10

fun remove_card(cs,c,e) =
  case cs of
      [] => raise e
    | x::xs' => if x = c then xs' else x::remove_card(xs',c,e)

fun all_same_color(cs) =
  case cs of
      [] => true
    | x::[] => true
    | x::xs'::[] => card_color(x) = card_color(xs')
    | x::xs'::xs'' => if card_color(x) = card_color(xs') then all_same_color(xs'::xs'') else false

fun sum_cards(cs) =
  let fun helper(cl, acc) =
	case cl of
	    [] => acc
	  | x::xs' => helper(xs', card_value(x) + acc)
  in helper(cs,0)
  end

fun score(cs,g) =
  let val divider = if all_same_color(cs) then 2 else 1
      val sum = sum_cards(cs)
      val prescore = if sum > g then 3 * (sum - g) else g - sum
  in prescore div divider
  end
      
fun officiate(cds,moves,i) =
  let fun helper(c,m,h) =
	case (c,m,sum_cards(h) > i) of
	    (_,_,true) => score(h,i)
	  | (_,[],false) => score(h,i)
	  | (_,(Discard crd)::xs',false) => helper(c,xs',remove_card(h,crd,IllegalMove))
	  | ([],(Draw)::xs',false) => score(h,i)
	  | (x::xs',(Draw)::ys',false) => helper(xs',ys',x::h)
  in helper(cds,moves,[])
  end;

fun replace_one_ace(cs) =
  case cs of
      [] => []
    | (c, Ace)::xs' => (c,Num(1))::xs'
    | card::xs' => card::replace_one_ace(xs')

fun replace_one_ace_move(cs) =
  case cs of
      [] => []
    | Discard(c, Ace)::xs' => Discard(c,Num(1))::xs'
    | card::xs' => card::replace_one_ace_move(xs')					

fun least_of(ls) =
  case ls of
      [] => 0
    | x::[] => x
    | x::xs' => let val min = least_of(xs')
		in if min < x
		   then min
		   else x
		end;
				     
fun score_challenge(cs,g) =
  let fun container(cs) =
	let val replaced = replace_one_ace(cs)
	in if cs = replaced
	   then [score(cs,g)]
	   else score(cs,g)::container(replaced)
	end
  in least_of(container(cs))
  end;

fun officiate_challenge(cds,moves,i) =
  let fun container(cs, moves) =
	let val replaced = replace_one_ace(cs)
	    val repl_moves = replace_one_ace_move(moves)
	in if cs = replaced
	   then [officiate(cs,moves,i)]
	   else officiate(cs,moves,i)::container(replaced, repl_moves)
	end
   in least_of(container(cds, moves))
  end;

fun careful_player(cs,g) =
  let fun research_discard(held, new_card, total_value) =
	case held of
	    [] => NONE
	  | card::xs' => if total_value - card_value(card) + card_value(new_card) = 0
			 then SOME([Discard(card),Draw])
			 else research_discard(xs', new_card, total_value)
	  
      fun helper(cs,held,moves) =
	case (cs, score(held,g) = 0 andalso not(null(held))) of
	    (_,true) => moves
	  | ([],false) => moves
	  | (card::xs',false) => if sum_cards(held) <= g + 11
				 then helper(xs',held@[card],moves@[Draw])
				 else case research_discard(held, card, sum_cards(held)) of
					  NONE => moves
					| SOME(move) => helper(xs',held@[card],moves@move)
  in helper(cs,[],[])
  end;

use "johnson_hw2_test.sml";

(*

*)
