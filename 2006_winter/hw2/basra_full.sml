(* datatypes and types and card dealing *)

datatype suit = Spade | Heart | Diamond | Club
type rank = int
type card = suit * rank
type cards = card list

fun sort(lst: cards) = (* implement your sort function here *)


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
fun capture(floor: cards, card: card) = (* implement your capture function here *)

(* Game Over *)
fun isGameOver (deck: cards, floor: cards, hand: cards) = (* implement your isGameOver function here *)

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
fun produceShuffledDeck() = (* implement your produceShuffledDeck function here *)

(* Main Program of Basra Game *)
structure Test : sig
    val main : (string * string list) -> OS.Process.status
end = struct

(* ************* BEGIN promptChoice ************ *)

fun promptChoice() = (fn NONE => ~1 | SOME s => valOf(Int.fromString(s)))(TextIO.inputLine(TextIO.stdIn))

fun promptChoice1(low, high) = (
    print "Please select card to play (1-4, or 0 to quit): ";
    let
        val tmp = promptChoice();
    in
        if tmp = 0 then 0 else (
            if tmp <= high andalso tmp >= low then tmp
            else promptChoice1(low, high))
    end
);

(* ************* END promptChoice ************ *)

fun deal (0, hand: cards) = nil: cards 
  | deal (n, hand: cards) = hd(hand)::deal(n-1, tl(hand));

(* ************* BEGIN displays ************ *)

fun displayHand (hand) = (
    print "Your Hand currently contains:\n";
    print(displayHand2(hand, 1))
)
and
    displayHand2 (nil: cards, _) = "\n" |
    displayHand2 ((s: card)::st, n) = 
         "\t(" ^ Int.toString(n) ^ ".)" ^ getSuit(#1 s) ^ " " ^ Int.toString(#2 s) ^ " " ^ displayHand2(st, n+1)      
and
    displayFloor (hand) = (
    print "The Floor currently contains:\n";
    print(displayFloor2(hand))        
)
and
    displayFloor2 (nil) = "" |
    displayFloor2 ((s: card)::st) = 
      "\t" ^ getSuit(#1 s) ^ " " ^ Int.toString(#2 s) ^ "\n" ^ displayFloor2(st)
and
    getSuit (Spade) = "Spade  " |
    getSuit (Heart) = "Heart  " |
    getSuit (Diamond) = "Diamond" |
    getSuit (Club) = "Club   ";

(* ************* END displays ************ *)

fun getCard (hand: cards, 0) = hd(hand) 
  | getCard (hand: cards, 1) = hd(hand) 
  | getCard (hand: cards, n) = getCard(tl(hand), n-1);

fun unwind (nil: cards list) = nil: cards |
    unwind (s::st) = s @ unwind(st);(* Create the random number generator *)
fun Seeder() = let val time = Time.toMilliseconds(Time.now())
		   val time' = case Int.maxInt
			       of NONE => time
				| SOME x => LargeInt.mod(time, Int.toLarge x)
	       in Int.fromLarge time'
	       end

val RandGen = Random.rand(Seeder(), Seeder())

(* Helper Function 2 for produceShuffledDeck: generates a random integer *)
fun getRandomInt(max: int) = 
    if max = 0 then NONE 
    else SOME((Random.randInt(RandGen) mod max) + 1) (* Keep the +1 if you want it to generate from 1 to max, or remove it to generate from 0 to max - 1 *)

fun play () = (
    let
         val Deck = produceShuffledDeck();
         val Hand = deal(4, Deck);
         val Deck = removeCardsFromHand(Deck, Hand); 
         val Floor = deal(4, Deck);
         val Deck = removeCardsFromHand(Deck, Floor);
         val Scored = ref(nil: cards); 
         val score = ref(0);
         val sweep = ref(0) 
         val Deck = ref(Deck);
         val Hand = ref(Hand);
         val Floor = ref(Floor);
         val choice = ref(0);
         val captured = ref(nil: cards list);
         val tmp = ref((Spade,~1));
         val lst = ref(nil: cards);
    in
      while (isGameOver(!Deck, !Floor, !Hand)=false) do (   
         displayFloor(!Floor); 
         displayHand(!Hand);
         
         choice := promptChoice1(1, 4);
         if (!choice)=0 then endGame(!Scored, !sweep) else (
          
         tmp := getCard(!Hand, !choice);
         captured := capture(!Floor, !tmp);

         if null(!captured) then
             if length(!Floor)>=6 then endGame(!Scored, !sweep)
             else Floor:=addToHand(!Floor, !tmp) 
         else ( Scored := addCardsToHand(!Scored, (!tmp)::unwind(!captured));
                Floor := removeCardsFromHand(!Floor, unwind(!captured));
                print("\nCaptured from the floor cards:\n" ^ displayFloor2(unwind(!captured)))
         );

         Hand := removeFromHand(!Hand, !tmp);

         (* see if the capture makes a "basra"/"sweep" *)
         sweep := !sweep + (fn nil => 1 | _ => 0)(!Floor);

         (* deal a card to the player *)
         lst := deal(1, !Deck);
         Hand := addCardsToHand(!Hand, !lst);
         Deck := removeCardsFromHand(!Deck, !lst);

         (* refill Floor is necessary *)
         lst := deal(handHunger(!Floor), !Deck);
         Floor := addCardsToHand(!Floor, !lst);
         Deck := removeCardsFromHand(!Deck, !lst)
         ) (* if-else of choice *)
      ); 
         endGame(!Scored, !sweep)
    end    
)
and
    endGame (hand, sweep) = (
         print("\nGame Over\n");
         print("Scored stack:\n" ^ displayFloor2(sort(hand)) ^ "\n");
         print("Total points earned in this round: " ^ Int.toString(earnedScore(hand)+3*sweep) ^ "\n");
         OS.Process.terminate OS.Process.success
          
)
   
fun main(name, args) =
    let       
    in
         play();                           
         OS.Process.success
    end
end

val _ = SMLofNJ.exportFn("Basra", Test.main);
