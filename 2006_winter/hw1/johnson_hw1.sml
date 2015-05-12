val Spade = 1;
val Heart = 2;
val Diamond = 3;
val Club = 4;
val Jack = 11;
val Queen = 12;
val King = 13;

fun isValidHand hand = not (List.exists (fn(s,r)=> s < 1 orelse s > 4 orelse r < 1 orelse r > 13)
					hand)
			   
			      
fun isValidHand_long [] = true
  | isValidHand_long ((s,r)::hs') = s > 0 andalso s < 5 andalso r > 0 andalso r < 14 andalso (isValidHand_long hs')
												 
fun handHunger h = let val n = List.length(h) in if n < 4 then 4-n else 0 end

fun handHunger_long h =
  let fun count [] = 0
	| count (h::hs') = 1 + (count hs')
      val n = count h
  in if n < 4 then n-4 else 0
  end
								    
fun addToHand c h = let val n = List.length(h) in if n > 5 then [] else c::h end
			
fun removeFromHand c [] = []
  | removeFromHand c (h::hs') = if c=h then hs' else h::(removeFromHand c hs')
							    
fun removeCardsFromHand c [] = []
  | removeCardsFromHand [] h = h
  | removeCardsFromHand (c::cs') h = removeCardsFromHand cs' (removeFromHand c h)

fun earnedScore [] = 0
  | earnedScore ((_,r)::hand) = case r of
				    11 => 2 + (earnedScore hand)
				  | 12 => 2 + (earnedScore hand)
				  | 13  => 2 + (earnedScore hand)
				  | _ => 1 + (earnedScore hand);
isValidHand([(1, 3), (2, 4)]) = true : bool;
isValidHand([(5, 3), (1, 14)]) = false : bool;
handHunger([(1, 3), (2, 4)]) =2;
handHunger([(1, 3), (2, 4), (4, 11), (2, 1), (1, 13)])=0;
addToHand (1, 3) [(2, 1), (2, 11)] = [(1, 3), (2, 1), (2, 11)];
addToHand (1, 3) [(3, 3), (2, 4), (4, 11), (2, 1), (1, 13), (3,5)] =[];
removeFromHand (1, 3) [(1, 11), (2, 4), (1, 3)] = [(1,11),(2,4)];
removeCardsFromHand [(2, 4), (2, 1)] [(1, 11), (2, 4), (1, 3)] = [(1,11),(1,3)];
removeCardsFromHand[(2, 4), (2, 1)] [(1, 11), (2, 4), (1, 3)] = [(1,11),(1,3)];
earnedScore([(1, 3), (1, 11), (2, 13)]) =5;
