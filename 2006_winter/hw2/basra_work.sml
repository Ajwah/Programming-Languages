(* datatypes and types and card dealing *)

datatype suit = Spade | Heart | Diamond | Club
type rank = int;
type card = suit * rank;
type cards = card list;

fun sort (lst: cards) = ( (* implement your sort function here *) )

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
fun capture(floor: cards, card: card) = ( (* implement your capture function here *) )

(* Game Over *)
fun isGameOver (deck: cards, floor: cards, hand: cards) = ( (* implement your isGameOver function here *) )

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
	else SOME((Random.randInt(RandGen) mod max) + 1) (* Keep the +1 if you want it to generate from 1 to max, or remove it to generate from 0 to max - 1 *)
    end

(* produceShuffledDeck returns a shuffled deck of 52 distinct cards *)
fun produceShuffledDeck() = ( (* implement your produceShuffledDeck function here *) )

