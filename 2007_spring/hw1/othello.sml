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
;
  val init = (~1,[((4,4),~1),((4,5),1),((5,4),1),((5,5),~1)]);
val hori_l = (~1,[((3,4),~1),((4,4),1),((5,4),1),((6,4),1)]) and hori_l_gap = (7,4) and hori_l_dir = (~1,0);
val hori_r = (~1,[((7,4),~1),((4,4),1),((5,4),1),((6,4),1)]) and hori_r_gap = (3,4) and hori_r_dir = (1,0);
val vert_u = (~1,[((4,8),~1),((4,5),1),((4,6),1),((4,7),1)]) and vert_u_gap = (4,4) and vert_u_dir = (0,1);
val vert_d = (~1,[((4,4),~1),((4,5),1),((4,6),1),((4,7),1)]) and vert_d_gap = (4,8) and vert_d_dir = (0,~1);
val scant1_d = (~1,[((2,2),~1),((3,3),1),((4,4),1),((5,5),1)]) and scant1_d_gap = (6,6) and scant1_d_dir = (~1,~1);
val scant1_u = (~1,[((6,6),~1),((3,3),1),((4,4),1),((5,5),1)]) and scant1_u_gap = (2,2) and scant1_u_dir = (1,1);
val scant2_u = (~1,[((1,8),~1),((2,7),1),((3,6),1),((4,5),1)]) and scant2_u_gap = (5,4) and scant2_u_dir = (~1,1);
val scant2_d = (~1,[((5,4),~1),((2,7),1),((3,6),1),((4,5),1)]) and scant2_d_gap = (1,8) and scant2_d_dir = (1,~1);

val _ = show init;

val name_hw = "2007 - Spring - Assignment One: Othello"

val f1 = flanks;
(*Unit tests are of format string*bool where bool is represented by evaluation of a function to an expected value*)
val tests = [
    ("0.01", f1 (0,0) init (1,0) = 1000 handle UnsoundPosition => true),
    ("0.02", f1 (9,9) init (1,0) = 1000 handle UnsoundPosition => true),
    ("0.03", f1 (1,1) (0,[]) (1,0) = 1000 handle UnsoundState => true),
    ("0.04", f1 (1,1) (1,[((1,1),5)]) (1,0) = 1000 handle UnsoundState => true),
    ("0.05", f1 (1,1) (1,[((0,1),1)]) (1,0) = 1000 handle UnsoundState => true),
    ("0.06", f1 (1,1) (1,[((0,1),~5)]) (1,0) = 1000 handle UnsoundState => true),
    ("0.07", f1 (1,1) init (0,0) = 1000 handle UnsoundDirection => true),
    ("0.08", f1 (1,1) init (2,1) = 1000 handle UnsoundDirection => true),

    (*check left*)
    ("0.10", f1 (3,5) init (1,0) = 1),
    ("0.11", f1 (3,5) init (~1,0) = 0),
    ("0.12", f1 (3,5) init (0,1) = 0),
    ("0.13", f1 (3,5) init (0,~1) = 0),
    ("0.14", f1 (3,5) init (1,1) = 0),
    ("0.15", f1 (3,5) init (1,~1) = 0),
    ("0.16", f1 (3,5) init (~1,1) = 0),
    ("0.17", f1 (3,5) init (~1,~1) = 0),

    (*check left top*)
    ("0.20", f1 (3,6) init (1,0) = 0),
    ("0.21", f1 (3,6) init (~1,0) = 0),
    ("0.22", f1 (3,6) init (0,1) = 0),
    ("0.23", f1 (3,6) init (0,~1) = 0),
    ("0.24", f1 (3,6) init (1,1) = 0),
    ("0.25", f1 (3,6) init (1,~1) = 0),
    ("0.26", f1 (3,6) init (~1,1) = 0),
    ("0.27", f1 (3,6) init (~1,~1) = 0),

    (*check middle-left top*)
    ("0.30", f1 (4,6) init (1,0) = 0),
    ("0.31", f1 (4,6) init (~1,0) = 0),
    ("0.32", f1 (4,6) init (0,1) = 0),
    ("0.33", f1 (4,6) init (0,~1) = 1),
    ("0.34", f1 (4,6) init (1,1) = 0),
    ("0.35", f1 (4,6) init (1,~1) = 0),
    ("0.36", f1 (4,6) init (~1,1) = 0),
    ("0.37", f1 (4,6) init (~1,~1) = 0),
    
    (*check middle-right top*)
    ("0.40", f1 (5,6) init (1,0) = 0),
    ("0.41", f1 (5,6) init (~1,0) = 0),
    ("0.42", f1 (5,6) init (0,1) = 0),
    ("0.43", f1 (5,6) init (0,~1) = 0),
    ("0.44", f1 (5,6) init (1,1) = 0),
    ("0.45", f1 (5,6) init (1,~1) = 0),
    ("0.46", f1 (5,6) init (~1,1) = 0),
    ("0.47", f1 (5,6) init (~1,~1) = 0),

    (*check right top*)
    ("0.50", f1 (6,6) init (1,0) = 0),
    ("0.51", f1 (6,6) init (~1,0) = 0),
    ("0.52", f1 (6,6) init (0,1) = 0),
    ("0.53", f1 (6,6) init (0,~1) = 0),
    ("0.54", f1 (6,6) init (1,1) = 0),
    ("0.55", f1 (6,6) init (1,~1) = 0),
    ("0.56", f1 (6,6) init (~1,1) = 0),
    ("0.57", f1 (6,6) init (~1,~1) = 0),

    (*check right*)
    ("0.60", f1 (6,5) init (1,0) = 0),
    ("0.61", f1 (6,5) init (~1,0) = 0),
    ("0.62", f1 (6,5) init (0,1) = 0),
    ("0.63", f1 (6,5) init (0,~1) = 0),
    ("0.64", f1 (6,5) init (1,1) = 0),
    ("0.65", f1 (6,5) init (1,~1) = 0),
    ("0.66", f1 (6,5) init (~1,1) = 0),
    ("0.67", f1 (6,5) init (~1,~1) = 0),

    (*check right*)
    ("0.70", f1 (6,4) init (1,0) = 0),
    ("0.71", f1 (6,4) init (~1,0) = 1),
    ("0.72", f1 (6,4) init (0,1) = 0),
    ("0.73", f1 (6,4) init (0,~1) = 0),
    ("0.74", f1 (6,4) init (1,1) = 0),
    ("0.75", f1 (6,4) init (1,~1) = 0),
    ("0.76", f1 (6,4) init (~1,1) = 0),
    ("0.77", f1 (6,4) init (~1,~1) = 0),

    (*check right bottom*)
    ("0.80", f1 (6,3) init (1,0) = 0),
    ("0.81", f1 (6,3) init (~1,0) = 0),
    ("0.82", f1 (6,3) init (0,1) = 0),
    ("0.83", f1 (6,3) init (0,~1) = 0),
    ("0.84", f1 (6,3) init (1,1) = 0),
    ("0.85", f1 (6,3) init (1,~1) = 0),
    ("0.86", f1 (6,3) init (~1,1) = 0),
    ("0.87", f1 (6,3) init (~1,~1) = 0),

    (*check middle bottom*)
    ("0.90", f1 (5,3) init (1,0) = 0),
    ("0.91", f1 (5,3) init (~1,0) = 0),
    ("0.92", f1 (5,3) init (0,1) = 1),
    ("0.93", f1 (5,3) init (0,~1) = 0),
    ("0.94", f1 (5,3) init (1,1) = 0),
    ("0.95", f1 (5,3) init (1,~1) = 0),
    ("0.96", f1 (5,3) init (~1,1) = 0),
    ("0.97", f1 (5,3) init (~1,~1) = 0),

    (*check middle bottom to left*)
    ("0.100", f1 (4,3) init (1,0) = 0),
    ("0.101", f1 (4,3) init (~1,0) = 0),
    ("0.102", f1 (4,3) init (0,1) = 0),
    ("0.103", f1 (4,3) init (0,~1) = 0),
    ("0.104", f1 (4,3) init (1,1) = 0),
    ("0.105", f1 (4,3) init (1,~1) = 0),
    ("0.106", f1 (4,3) init (~1,1) = 0),
    ("0.107", f1 (4,3) init (~1,~1) = 0),

    (*check left bottom bottom*)
    ("0.110", f1 (3,3) init (1,0) = 0),
    ("0.111", f1 (3,3) init (~1,0) = 0),
    ("0.112", f1 (3,3) init (0,1) = 0),
    ("0.113", f1 (3,3) init (0,~1) = 0),
    ("0.114", f1 (3,3) init (1,1) = 0),
    ("0.115", f1 (3,3) init (1,~1) = 0),
    ("0.116", f1 (3,3) init (~1,1) = 0),
    ("0.117", f1 (3,3) init (~1,~1) = 0),

    (*check left bottom*)
    ("0.120", f1 (3,4) init (1,0) = 0),
    ("0.121", f1 (3,4) init (~1,0) = 0),
    ("0.122", f1 (3,4) init (0,1) = 0),
    ("0.123", f1 (3,4) init (0,~1) = 0),
    ("0.124", f1 (3,4) init (1,1) = 0),
    ("0.125", f1 (3,4) init (1,~1) = 0),
    ("0.126", f1 (3,4) init (~1,1) = 0),
    ("0.127", f1 (3,4) init (~1,~1) = 0),

    ("1.00", f1 hori_l_gap hori_l hori_l_dir = 3),
    ("1.01", f1 hori_r_gap hori_r hori_r_dir = 3),
    ("1.02", f1 vert_u_gap vert_u vert_u_dir = 3),
    ("1.03", f1 vert_d_gap vert_d vert_d_dir = 3),
    ("1.04", f1 scant1_d_gap scant1_d scant1_d_dir = 3),
    ("1.05", f1 scant1_u_gap scant1_u scant1_u_dir = 3),
    ("1.06", f1 scant2_u_gap scant2_u scant2_u_dir = 3),
    ("1.07", f1 scant2_d_gap scant2_d scant2_d_dir = 3)
];
 
print ("\n"^Int.toString(List.length(tests))^" TOTAL TESTS RUN----------------------"^name_hw^"--------------------------\n"); (*Name display to assert correct test file is running*)
fun all_tests(tests) =
	let fun helper(tests: (string*bool) list, all_passed) = 
		case tests of
			[] => all_passed
	  	  | (st, true)::rest =>  helper(rest, all_passed) (*I am only interested in the output if the test fails.*)
	  	  | (st, false)::rest => (print ("***" ^ st ^" !!!FAILED!!!\n"); helper(rest, false))
	in
		helper(tests, true)
	end;
 
case all_tests(tests) of
    true => print "--------------EVERY TESTS PASSED-------------\n"
  | false => print "--------------SOMETHING IS WRONG-------------------------\n";

val next1 = move (3,5) init;   show next1;
val next2 = move (3,6) next1;  show next2;
val next3 = move (5,3) next2;  show next3;
val next4 = move (4,3) next3;  show next4;
val next5 = move (3,3) next4;  show next5;
val next6 = move (4,2) next5;  show next6;
val next7 = move (2,7) next6;  show next7;
val next8 = move (6,4) next7;  show next8;
val next9 = move (6,3) next8;  show next9;
