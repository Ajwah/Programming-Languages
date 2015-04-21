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
      val prescore = if sum > g then sum - g else g - sum
  in prescore div divider
  end
      
fun officiate(c,m,i) = i;
use "johnson_hw2_test.sml";
