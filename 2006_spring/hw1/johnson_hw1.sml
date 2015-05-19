val maxX = 8
val maxY = 8
fun add_piece c p = p :: c
fun on_board (x,y) = let val t1 = x mod 2 = 0
			 val t2 = y mod 2 = 0
		     in if t2 then t1 else not t1
		     end
			 
fun legal_piece (x,y,s,_) = (s = 1 orelse s = ~1) andalso on_board(x,y)

fun piece_at [] _ = NONE
  | piece_at ((c as (cx,cy,_,_))::cs') (x,y) = if cx = x andalso cy = y then SOME c else piece_at cs' (x,y)

fun legal_config c =
    let fun isDuplicate [] _ = false
	  | isDuplicate (l::ls') p = if p=l then true else isDuplicate ls' p
	fun loop [] = true
	  | loop (c::cs') = legal_piece c andalso not (isDuplicate cs' c)
    in loop c
    end

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
	  ImpossibleError
fun legal_move c (p as (px,py,ps,pk))  (d as (dx,dy)) =
		  let val moved as (mx,my) = (px+dx,px+dy)
		      val assert_LegalConfig = if legal_config c then true else raise IllegalConfig
		      val assert_PlausibleMove = if abs(dx) = abs(dy) then true else raise ImplausibleMove
		      val assert_BoardRange = if (mx> ~1 andalso mx<maxX andalso my> ~1 andalso my<maxY andalso px> ~1 andalso px<maxX andalso py> ~1 andalso py<maxY) then true else raise OutOfBoardRange
		      val isCorrectPlayingDirectionHelper = if not pk then 
								if ps=1 then dy=abs(dy) else dy<>abs(dy)
							    else true
		      val assert_CorrectPlayingDirection = if isCorrectPlayingDirectionHelper then true else raise IncorrectPlayingDirection
		      val assert_DestinationOccupied = if piece_at c moved = NONE then false else raise DestinationOccupied
		      val assert_P_in_C = if List.exists (fn(e)=>e=p) c then true else raise PieceNotPresent
								       
		      val atomicStep as (ax,ay) = (dx div abs(dx), dy div abs(dy)) handle Div => raise ZeroMove
		      							
		      fun createSteppingRange (step as (x,y)) acc counterOccupied (enemyCoord: (int*int*int*bool) option) =
			let val nextStep = (x+ax,y+ay)
			    val isOccupied = piece_at c nextStep
			    val counter = if isOccupied = NONE then 0 else 1
			    val enemyCo = if enemyCoord = NONE then isOccupied else enemyCoord
											
			    fun assert_InterspersingIsEnemyOrEmpty NONE = true
			      | assert_InterspersingIsEnemyOrEmpty (SOME (_,_,es,_)) = if ps <> es then true else raise NotEnemy
			    val _ = assert_InterspersingIsEnemyOrEmpty enemyCo
			in if abs(x)<abs(mx) andalso abs(y)<abs(my)
			   then createSteppingRange nextStep ((nextStep,isOccupied)::acc) (counterOccupied+counter) enemyCo
			   else (acc,counterOccupied,enemyCoord)
			end
			    
		      val (rangeOfSteps,piecesInbetween,enemyCoord) = createSteppingRange (px,py) [] 0 NONE
		      val isMultiplePiecesInterspersing = if piecesInbetween > 1 then raise MultiplePiecesInterspersing else false
						      
		      val isJump = piecesInbetween = 1 andalso length rangeOfSteps >= 2 (*if piecesInbetween = 1 then
				       if pk then abs(dx) >= 2 andalso abs(dy) >= 2 else abs(dx) = 2 andalso abs(dy) = 2
				   else false*)
		      val isSlide = piecesInbetween = 0 andalso length rangeOfSteps >= 1 (*if piecesInbetween = 0 then
					if pk then abs(dx) >= 1 andalso abs(dy) >= 1 else abs(dx) = 1 andalso abs(dy) = 1
				    else false*)
		  in case pk of
			 false => if isSlide andalso length rangeOfSteps = 1 then [((mx,my,ps,pk),[])] else
				  if length rangeOfSteps = 0 then raise ImpossibleError else (*Impossible as handled by ZeroMove above*)
				  if isSlide andalso length rangeOfSteps > 1 then raise OnlyOneStep else
				  if isJump andalso length rangeOfSteps = 2 then [((mx,my,ps,pk),[enemyCoord])] else raise OnlyTwoStepJump
															   
		       | true  => if isSlide then [((mx,my,ps,pk),[])] else
				  if length rangeOfSteps = 0 then raise ImpossibleError else (*Impossible as handled by ZeroMove above*)
				  if isJump then [((mx,my,ps,pk),[enemyCoord])] else raise ImpossibleError
		  end
