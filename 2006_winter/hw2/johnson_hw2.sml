(* datatypes and types and card dealing *)

datatype suit = Spade | Heart | Diamond | Club
type rank = int;
type card = suit * rank;
type cards = card list;

fun sort [] = []
  | sort [c] = [c]
  | sort (lst: cards) =
  let val ln = length lst
      val mid = ln div 2
      val p as (ps,pr) = List.nth (lst,mid)
      fun isBigger (s1,r1) (s2,r2) =
	if s1 = s2 then r1 > r2 else
	case (s1,s2) of
	    (Spade,_) => false
	  | (Heart,Spade) => true
	  | (Heart,_) => false
	  | (Diamond,Spade) => true
	  | (Diamond,Heart) => true
	  | (Diamond,_) => false
	  | (Club,_) => true
      val left = List.filter (fn(c as (cs,cr))=> not (cs = ps andalso cr = pr) andalso (not (isBigger c p))) lst
      val right = List.filter (fn(c)=> isBigger c p) lst
  in 
      sort(p::left)@(sort(right))
  end
		   
(* Part 1 of Basra Game *)

(* 1 - 2 *)
fun handHunger (hand: cards) = if length hand >= 4 then 0 else 4 - length hand;

(* 1 - 3 *)
fun addToHand (hand: cards, card: card) = if length hand < 6 then card::hand else nil;

(* extra *)
fun addCardsToHand (hand: cards, cards: cards) = case cards of [] => hand
							     | head :: tail => addCardsToHand(head :: hand, tail)
(* 1 - 4 *)
fun removeFromHand (hand: cards, card: card) = case hand of [] => []
							  | (s: card) :: st => if (card = s) then st else s::removeFromHand(st, card)

(* 1 - 5 *)
fun removeCardsFromHand (nil, _) = nil 
  | removeCardsFromHand (hand: cards, nil) = hand 
  | removeCardsFromHand (hand: cards, (c: card)::cs) = removeCardsFromHand(removeFromHand(hand, c), cs);

(* 1 - 6 *)
fun earnedScore (nil) = 0 
  | earnedScore ((s: card)::st) = (
      let
            val x = if #2(s) >= 11 then 2 else 1
      in
            x + earnedScore(st)
      end
);

(* Part 2 of Basra Game *)

(* Capture *)
fun produceAllCaptures(floor: cards, card as (cs,cr): card) = 
  let val summation = List.foldl (fn((_,cr),acc)=> cr+acc) 0  
      fun combinations [] = []
      | combinations (head::[]) = [[head]]
      | combinations (head::neck::tail) =
	let fun fold a [] = []
	      | fold a (l::ls') = (a@[l],ls')::(fold a ls')
	    fun enum [] = []
	      | enum ((l1,l2)::ls') = l1::(enum(fold l1 l2)) @ enum ls'
	in enum [([head],neck::tail)] @ combinations (neck::tail)
	end;
in
    List.filter (fn(cl)=> summation cl = cr) (combinations floor)
end

(* Game Over *)
fun isGameOver (deck: cards, floor: cards, hand: cards) =
  case (deck,floor,hand) of
      ([],[],_) => true
    | (_,_,[]) => true
    | (_,floor,hand) => List.length (floor) = 6 andalso (List.foldl (fn(c,acc)=> produceAllCaptures(floor,c)@acc) [] hand) = []

(* Helper Function 1 for produceShuffledDeck: Generate a shuffled deck *)
fun produceUnshuffledDeck() =
    let
	fun generateCardsInSuit(suit: suit, deck: cards, currentRank: int) =
	    if currentRank > 13 then deck
	    else generateCardsInSuit(suit, (suit, currentRank) :: deck, currentRank + 1)
	val deck1 = generateCardsInSuit(Spade, [], 1)
	val deck2 = generateCardsInSuit(Heart, deck1, 1)
	val deck3 = generateCardsInSuit(Diamond, deck2, 1)
	val deck4 = generateCardsInSuit(Club, deck3, 1)
    in
	deck4
    end

(* Helper Function 2 for produceShuffledDeck: generates a random integer *)
fun getRandomInt(max: int) =
    let
	fun Seeder() = let val time = Time.toMilliseconds(Time.now())
			   val time' = case Int.maxInt
					of NONE => time
					 | SOME x => LargeInt.mod(time, Int.toLarge x)
		       in Int.fromLarge time'
		       end
	val RandGen = Random.rand(Seeder(), Seeder())
    in
	if max = 0 then NONE 
	else SOME((Random.randInt(RandGen) mod max)) (* Keep the +1 if you want it to generate from 1 to max, or remove it to generate from 0 to max - 1 *)
    end

(* produceShuffledDeck returns a shuffled deck of 52 distinct cards *)
fun produceShuffledDeck() =
  let val initial_deck = produceUnshuffledDeck()
      fun shuffle deck initial_deck =
	  let val SOME rnd = getRandomInt(52)
	      val rnd_card as (s,r) = List.nth(initial_deck,rnd)
	      val isDuplicate = List.exists (fn(c)=>rnd_card = c) deck
	      val isComplete = (List.length deck) = 52
	  in if isComplete then deck else if isDuplicate then shuffle deck initial_deck else shuffle (rnd_card::deck) initial_deck
	  end
      fun loop 0 shuffled = shuffled
	| loop n sh = loop (n-1) (shuffle [] sh)
  in loop 7 initial_deck
  end

