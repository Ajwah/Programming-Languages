fun legalpos (x,y) = x>0 andalso x <9 andalso y>0 andalso y<9
fun color (pos as (x,y)) (state as (turn, [])) = 0 
  | color (pos as (x,y)) (state as (turn, (piece_coord,c)::board)) = if piece_coord = pos then if legalpos pos then c else 0 else color pos (turn, board)
fun color2 (pos as (x,y)) (state as (turn, board)) = case List.find (fn(coord,_)=>pos=coord) board of
							NONE => 0
						      | SOME (coord,c) => if legalpos coord then c else 0
fun emptysquare pos state = color pos state = 0
fun is_sound_state (t,lst) = (t=1 orelse t= ~1) andalso List.foldl (fn((coord,col),acc) => (col= ~1 orelse col=1) andalso legalpos coord andalso acc) true lst
fun is_sound_direction (dx,dy) = dx>= ~1 andalso dx<=1 andalso dy>= ~1 andalso dy<=1
										       
exception UnsoundState and UnsoundDirection
fun flanks (pos as px,py) (state as (t,brd)) (dir as (dx,dy)) =
  let val assert_sound_state = if is_sound_state state then true else raise UnsoundState
      val assert_sound_direction = if is_sound_direction dir then true else raise UnsoundDirection
      fun count (pos as (x,y)) acc =
	let val piece = List.find (fn(coord,c)=> pos=coord) brd
	    val (is_stop,is_count) = case piece of
					 NONE => (true,false)
				       | SOME (coord,c) => (c<>t,true)
	in if is_stop then if is_count then length acc else 0 else count (x+dx,y+dy) (pos :: acc)
	end
  in count (px+dx,py+dy) []
  end

fun show_piece ((x,_),c) =
  let val prepend = if x=1 then "\n                               " else " "
  in prepend ^ (case c of
		   ~1 => "*"
		 | 0 => "."
		 | 1 => "o")
  end
			       
fun show (s as (t,brd)) =
  let fun loop 8 1 = show_piece ((8,1),color (8,1) s)^"\n"
	| loop 8 y = show_piece ((8,y),color (8,y) s)^loop 1 (y-1)
	| loop x y = show_piece ((x,y),color (x,y) s)^loop (x+1) y
      val msg = "\n"^ show_piece((5,5),t)^" to move; board:"
  in print (msg^ loop 1 8)
  end
;

  val init = (~1,[((4,4),~1),((4,5),1),((5,4),1),((5,5),~1)]);
