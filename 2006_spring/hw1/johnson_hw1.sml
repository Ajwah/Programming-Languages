val maxX = 8
val maxY = 8
	       
fun add_piece c p = p :: c
fun on_board (x,y) = let val t1 = x mod 2 = 0
			 val t2 = y mod 2 = 0
			 val t3 = if t2 then t1 else not t1
			 val t4 = x> ~1 andalso x< maxX andalso y> ~1 andalso y< maxY
		     in t3 andalso t4
		     end
			 
fun legal_piece (x,y,s,_) = (s = 1 orelse s = ~1) andalso on_board(x,y)

fun piece_at [] _ = NONE
  | piece_at ((c as (cx,cy,_,_))::cs') (x,y) = if cx = x andalso cy = y then SOME c else piece_at cs' (x,y)

fun legal_config c =
    let fun isDuplicate [] _ = false
	  | isDuplicate ((x,y,_,_)::ls') (p as (px,py,_,_)) = if px=x andalso py=y then true else isDuplicate ls' p
	fun loop [] = true
	  | loop (c::cs') = legal_piece c andalso not (isDuplicate cs' c) andalso loop cs'
    in loop c
    end

fun draw_piece (p as (_,_,pr,pk))  =
  case (pr,pk) of
      (1,false) => "o"
    | (1,true) => "O"
    | (~1,false) => "x"
    | (~1,true) => "X"

fun convert2str (p as (px,py,pr,pk)) =
  let val repr = draw_piece p
      val coord = " ("^Int.toString(px)^","^Int.toString(py)^")"
  in repr^coord
  end

fun toStr ((sp,ep,NONE),_) = convert2str sp ^" => "^convert2str ep^" Captured Piece: NONE"
  | toStr ((sp,ep,SOME cp),_) = convert2str sp ^" => "^convert2str ep^ " Captured Piece: "^convert2str cp
      
fun draw_board c =
  let fun loop x y =
	let (*val printxy = (print ("\n "^(Int.toString(x))^","^(Int.toString(y))))*)
	    val isP = List.find (fn(px,py,pr,pk) => px=x andalso py=y) c
	    val b as block =  case isP of
				  NONE => if on_board (x,y) then "#" else " "
			       |  SOME p => draw_piece p
	    val isNewLine = x=maxX-1
	    val isLastLine = y<0
	    val next = if isLastLine then "" else b ^ (if isNewLine then "\n"^ loop 0 (y-1) else loop (x+1) y)
	in next
	end
  in loop 0 (maxY-1)
  end	       
      
fun magnify_board b (mf as magnifying_factor)=
  let val t2lc as transform_string_to_list_chars = explode b 
      val enc as eliminate_newline_chars = List.filter (fn(e)=> e <> #"\n") t2lc
      val g8x8 as group_by_eight_X_eight = rev (#1 (List.foldl (fn(e,(acc,tmp,i))=> if i = maxX-1 then ((tmp@[e])::acc,[],0) else (acc,tmp@[e],i+1)) ([],[],0) enc))
      fun multiply_s s 0 = ""
	| multiply_s s n = s^(multiply_s s (n-1)) 
      fun loop x y m =
	let val cl as current_list = List.nth(g8x8,y)
	    val cc as current_char = List.nth(cl,x)
	    val cse as current_selection = case cc of
					       #" " => " "
					     | #"#" => "█"
					     | #"o" => "o"
					     | #"O" => "O"
					     | #"x" => "x"
					     | #"X" => "X"
							   
	    val cs as current_string = multiply_s cse (mf * 2)
	    val isNewLine = x=maxX-1
	    val r = if isNewLine then m+1 else m
	    val isNewCycle = r=mf
	    val isLastLine = y=maxY-1 andalso isNewCycle
	    val i = if isNewCycle then 0 else r
	    val cy as current_y = if isNewCycle then y + 1 else y
	in if isLastLine then "" else cs ^ (if isNewLine then "\n"^ loop 0 cy i else loop (x+1) cy i)
	end
  in loop 0 0 0
  end;

exception IllegalConfig and
	  ImplausibleMove and
	  IncorrectPlayingDirection and
	  OutOfBoardRange and
	  DestinationOccupied and
	  PieceNotPresent and
	  ZeroMove and
	  MultiplePiecesInterspersing and
	  NotEnemy and
	  OnlyOneStep and
	  OnlyTwoStepJump and
	  RangeOfStepsAnomaly and
	  ImpossibleError1 and
	  ImpossibleError2 and
	  ImpossibleError3 
	      
fun legal_move_core c (p as (px,py,ps,pk))  (d as (dx,dy)) =
  let val moved as (mx,my) = (px+dx,py+dy)
      val atomicStep as (ax,ay) = (dx div abs(dx), dy div abs(dy)) handle Div => raise ZeroMove
      (*val printer = (print ("\n p: ("^(Int.toString(px))^","^(Int.toString(py))^") d: ("^(Int.toString(dx))^","^(Int.toString(dy))^")"))*)
      val assert_LegalConfig = if legal_config c then true else raise IllegalConfig
      val assert_PlausibleMove = if abs(dx) = abs(dy) then true else raise ImplausibleMove
      val assert_BoardRange = if (mx> ~1 andalso mx<maxX andalso my> ~1 andalso my<maxY) then true else raise OutOfBoardRange
      val isCorrectPlayingDirectionHelper = if not pk then 
						if ps=1 then dy=abs(dy) else dy<>abs(dy)
					    else true
      val assert_CorrectPlayingDirection = if isCorrectPlayingDirectionHelper then true else raise IncorrectPlayingDirection
      val assert_DestinationNotOccupied = if piece_at c moved = NONE then false else raise DestinationOccupied
      val assert_P_in_C = if List.exists (fn(e)=>e=p) c then true else raise PieceNotPresent
		      							
      fun createSteppingRange (step as (x,y)) acc counterOccupied (enemyCoord: (int*int*int*bool) option) = 
	let val nextStep = (x+ax,y+ay)
	    val isOccupied = piece_at c nextStep
	    val counter = if isOccupied = NONE then 0 else 1
	    val enemyCo = if enemyCoord = NONE then isOccupied else enemyCoord
									
	    fun assert_InterspersingIsEnemyOrEmpty NONE = true
	      | assert_InterspersingIsEnemyOrEmpty (SOME (_,_,es,_)) = if ps <> es then true else raise NotEnemy
	    val _ = assert_InterspersingIsEnemyOrEmpty enemyCo
	in if x<>mx andalso y<>my
	   then createSteppingRange nextStep ((nextStep,isOccupied)::acc) (counterOccupied+counter) enemyCo 
	   else (acc,counterOccupied,enemyCoord)
	end
	    
      val (rangeOfSteps,piecesInbetween,enemyCoord) = createSteppingRange (px,py) [] 0 NONE
      val assert_rangeOfSteps = if length rangeOfSteps = 0 then raise RangeOfStepsAnomaly else true (* rangeOfSteps is to contain at least the destination*)
      val isMultiplePiecesInterspersing = if piecesInbetween > 1 then raise MultiplePiecesInterspersing else false
														 
      val isJump = piecesInbetween = 1 andalso length rangeOfSteps >= 2
      val isSlide = piecesInbetween = 0 andalso length rangeOfSteps >= 1
									   
  in case pk of
	 false => if isSlide andalso length rangeOfSteps = 1 then [(p,(mx,my,ps,pk),NONE)] else
		  if length rangeOfSteps = 0 then raise ImpossibleError1 else (*Impossible as handled by ZeroMove and RangeOfStepsAnomaly above*)
		  if isSlide andalso length rangeOfSteps > 1 then raise OnlyOneStep else
		  if isJump andalso length rangeOfSteps = 2 then [(p,(mx,my,ps,pk),enemyCoord)] else raise OnlyTwoStepJump
															   
       | true  => if isSlide then [(p,(mx,my,ps,pk),NONE)] else
		  if length rangeOfSteps = 0 then raise ImpossibleError2 else (*Impossible as handled by ZeroMove and RangeOfStepsAnomaly above*)
		  if isJump then [(p,(mx,my,ps,pk),enemyCoord)] else raise ImpossibleError3 (*If it is not isSlide then piecesInbetween has to be 1 as more than one is handled above by MultiplePiecesI                                                                                             nterspersing. Also, since it is not isSlide, then rangeOfSteps has to be at least two as negative is an imp                                                                                             ossible listlength and as 0 is handled above by RangeOfStepAnomaly and 1 is denied by it not being isSlide*)
  end 

fun legal_move c p d = legal_move_core c p d handle _ => []

fun all_single_moves [] _ = []
  | all_single_moves c r0 =
    let val pl_lst = List.filter (fn(_,_,r,_)=>r=r0) c
	val (kings,men) = List.partition (fn(_,_,_,k)=>k=true) pl_lst
	fun helper e r s = legal_move c e (s, s*r) @ legal_move c e (~s, s*r)
	fun handle_men [] = []
	  | handle_men (m::ms') = helper m r0 1 @ helper m r0 2 @ handle_men ms'
	fun handle_kings [] = []
	  | handle_kings (k::ks') =
	    let fun loop 0 = handle_kings ks'
		  | loop n = helper k 1 n @ helper k ~1 n @ loop (n-1)
	    in loop 7
	    end
    in handle_men men @ handle_kings kings
    end

exception CaptureNotPresent and EmptyBoard
				    
fun update_board [] _ = raise EmptyBoard
  | update_board c (sp as startingPiece, ep as endingPiece as (epx,epy,_,_), lc as listOfCaptures) =
    let val d as impossible_dummy_piece =  (~1,~1,100,false)
	val cp as captured_piece = case lc of NONE => d
					    | SOME p => p
	val assert_LegalConfig = if legal_config c then true else raise IllegalConfig
	val assert_PiecePresent = if List.exists (fn(e)=>e=sp) c then true else raise PieceNotPresent
	val assert_DestinationNotOccupied = if piece_at c (epx,epy) = NONE then true else raise DestinationOccupied
	val assert_BoardRange = if legal_piece ep then true else raise OutOfBoardRange
	val assert_CapturePresent = if cp <> d then
					if List.exists (fn(e)=>e=cp) c then true else raise CaptureNotPresent
				    else true
					     
	val updated = List.filter (fn(e)=>e<>sp andalso e<>cp) c
    in add_piece updated ep
    end

fun single_captures_by_p [] p = raise EmptyBoard
  | single_captures_by_p c (p as (_,_,pr,_)) =
    let val em as every_possible_move_on_board = all_single_moves c pr
	val captures_by_p = List.filter (fn(sp,_,lc)=> case lc of
							   NONE => false
							 | SOME piece => p = sp
					) em
    in captures_by_p
    end
	
fun all_captures_by_p [] _ = raise EmptyBoard
  | all_captures_by_p c (p as (_,_,pr,_)) =
    let val _ = (("\n Current_Board: \n"^magnify_board (draw_board c) 2))
	val lpc as possible_captures_by_p = single_captures_by_p c p
	val lpu as possible_updates_of_c = List.map (fn(s as state)=> update_board c s) lpc
	fun loop [] [] = []
	  | loop ((pc as (sp,ep,lc))::lpc') (pu::lpu') =
	    let val pri = List.foldl (fn(e,acc)=> acc ^"\n"^ (convert2str e)) "" pu
		val _ = (print ("\n "^(convert2str sp)^" ==> "^(convert2str ep)^" Captured Piece: "^(convert2str (valOf lc))^"\n"^(magnify_board (draw_board pu) 2)^pri))
	    in
		((pc,pu) :: all_captures_by_p pu ep) @ loop lpc' lpu'
	    end
    in loop lpc lpu
    end;

(*Reconstruct the various results obtained from all_captures_by_p to its various ordered branches
First the results obtained need to be grouped together under the common denominator of amount of pieces on board. Thus results are grouped together in groups of 1 piece only present, 2 pieces present, 3 pieces present etc. The less pieces will represent a further stage in the development of the game.
The end result should thus be: [[c(1)],[c(2)],[c(3)],..,[c(n)]] where [c(n)] is the list of all possible board configurations that share the same amount of pieces.
Second, starting from c(1) until c(n), the following must apply:
from c(i) => c(i+1) should be such that some elements in c(i+1) have as ending point that corresponds to some elements in c(i). c(i+1) may also contain elements that will not correspond, which will thus represent that one particular branch has been exhausted in level c(i+1) and could never logically evolve into c(i).
Both these are to be dealt with separately.

Third (still to be implemented): Actively pair up every instance from c(i) with the corresponding c(i+1). Those that do not branch further into c(i) should be included as is.

*)
fun sort_them c =
  let val d_l as different_lengths = List.foldl (fn(((_,_,_),board),acc)=> let val l = length board   
									   in if List.exists (fn(l')=>l'=l) acc then acc else l :: acc
									   end)
						[] c (* Output: [1,2,3,4,5] where each number corresponds to amount pieces on a board in differnt boards under c *)
      val b_a_p as by_amount_pieces = List.map (fn(l)=> List.filter (fn((_,_,_),board)=> length board=l) c) d_l (*Sort c according to different board lengths above, eg [1,2,3,4,5]*)
      val d_sp as different_starting_points = List.foldl (fn(((sp,_,_),_),acc)=> if List.exists (fn(sp')=>sp'=sp) acc then acc else sp :: acc)
							 [] (*Output : ((sp,ep,cp),board) => [sp1,sp2,sp3,..] 
							     Create a list of all the different starting points for a given list l*)
      fun branch (ml::ml'::mls') acc =
	let val b_csp as by_common_starting_points = List.foldl (fn(sp,acc)=>
								    (List.foldl (fn(e,acc)=>[e]::acc)
										[]
										(List.filter (fn((sp',_,_),_)=> sp'=sp) ml)
								    )@acc)
								[]
								(d_sp ml) (*Create a list of lists where every sub list occupies *)
	    val b_csp2 = List.foldl (fn(e,acc)=> [e]::acc) [] ml
	    val pair_ups as (pass,fail) = List.partition (fn((_,ep,_),_)=> List.exists (fn((sp,_,_),_)=> ep = sp) ml) ml'
	    val _ = List.map (fn([p as ((sp,ep,cp),b)])=> (print ("\n "^toStr p^"\n"^draw_board b);p)) b_csp2

	in (b_a_p,b_csp2)
	end
  in (branch b_a_p [])
  end;

print (magnify_board (draw_board [(3,3,~1,false),(2,2,1,false),(1,3,~1,false),(5,5,~1,false),(3,5,~1,false),(1,5,~1,false)]) 1);
val t = all_captures_by_p [(3,3,~1,false),(2,2,1,true),(1,3,~1,false),(5,5,~1,false),(3,5,~1,false),(1,5,~1,false)] (2,2,1,true);
val z = (sort_them t);
