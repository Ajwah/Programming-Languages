fun legalpos (x,y) = x>0 andalso x <9 andalso y>0 andalso y<9
fun color (pos as (x,y)) (state as (turn, [])) = 0 
  | color (pos as (x,y)) (state as (turn, (piece_coord,c)::board)) = if piece_coord = pos then if legalpos pos then c else 0 else color pos (turn, board)
fun color2 (pos as (x,y)) (state as (turn, board)) = case List.find (fn(coord,_)=>pos=coord) board of
							NONE => 0
						      | SOME (coord,c) => if legalpos coord then c else 0
fun emptysquare pos state = color pos state = 0
fun is_sound_state (t,lst) = (t=1 orelse t= ~1) andalso List.foldl (fn((coord,col),acc) => (col= ~1 orelse col=1) andalso legalpos coord andalso acc) true lst
val sound_directions = [(1,0),(~1,0),(0,1),(0,~1),(1,1),(1,~1),(~1,1),(~1,~1)];
fun is_sound_direction d = List.exists (fn(d')=> d=d') sound_directions

fun show_piece ((x,y),c) =
  let val prepend = if x=1 then "\n                               "^Int.toString(y) ^" " else " "
  in prepend ^ (case c of
		   ~1 => "*"
		 | 0 => "."
		 | 1 => "o")
  end

fun p2Str ((x,y),c) = show_piece ((5,5),c)^" ("^Int.toString(x)^","^Int.toString(y)^")"
fun s2Str (t,lst) = "to move: "^show_piece ((5,5),t)^" "^ List.foldl (fn(p,acc)=> acc^ p2Str p ^ "   ") "" lst
								     
exception UnsoundState and UnsoundDirection and UnsoundPosition
fun retrieve_converts (pos as (px,py)) (state as (t,brd)) (dir as (dx,dy)) =
  let val assert_sound_state = if is_sound_state state then true else raise UnsoundState
      val assert_sound_direction = if is_sound_direction dir then true else raise UnsoundDirection
      val assert_sound_position = if legalpos pos then true else raise UnsoundPosition
      fun count (pos as (x,y)) acc =
	let val piece = List.find (fn(coord,c)=> pos=coord) brd
	    val (is_stop,is_count) = case piece of
					 NONE => (true,false)
				       | SOME (coord,c) => (c=t,true)
	    val _ = ("\n"^ p2Str ((x,y),color (x,y) state)) (*for debugging purposes print out*)
	in if is_stop then if is_count then acc else [] else count (x+dx,y+dy) (pos :: acc)
	end
  in count (px+dx,py+dy) []
  end
      
fun flanks p s d = length (retrieve_converts p s d)
			  
fun move (p as (px,py)) (s as (t,brd)) =
  let val assert_sound_state = if is_sound_state s then true else raise UnsoundState
      val assert_sound_position = if legalpos p then true else raise UnsoundPosition
      val all_converts = List.concat (List.map (fn(d)=> retrieve_converts p s d) sound_directions)
      val (pass,fail) = List.partition (fn(co,_)=> List.exists (fn(co')=> co=co') all_converts) brd
      val change_them = List.map (fn(co,col)=> (co,t)) pass
      val new_board = (p,t) :: change_them @ fail
  in (~t,new_board)
  end
  
			       
fun show (s as (t,brd)) =
  let fun loop 8 1 = show_piece ((8,1),color (8,1) s)^"\n"
	| loop 8 y = show_piece ((8,y),color (8,y) s)^loop 1 (y-1)
	| loop x y = show_piece ((x,y),color (x,y) s)^loop (x+1) y
      val msg = "\n"^ show_piece((5,5),t)^" to move; board:"
  in print (msg^ loop 1 8)
  end
