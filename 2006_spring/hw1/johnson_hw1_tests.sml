use "johnson_hw1.sml";
val name_hw = "2006 - Spring - Assignment One: Checkers";
val p0 = (0,0,1,true) and q0 =(0,0);
val p1 = (1,1,1,false) and q1 = (1,1);
val p2 = (2,2,~1,false) and q2 = (2,2);
val p3 = (1,2,1,false) and q3 = (1,2);
val p4 = (2,1,1,false) and q4 = (2,1);
val p5 = (6,6,2,false) and q5 = (6,6);
val p6 = (3,5,1,false) and q6 = (3,5);
val p7 = (7,7,1,false) and q7 = (7,7);
val p8 = (10,20,1,false) and q8 = (10,20);
val p9 = (~10,0,1,false) and q9 = (~10,0);

val r0 = (5,3,1,false) and r1 = (1,1,~1,false);

val c0 = [];
val c1 = [p0];
val c2 = [p0,p7];
val c30 = [p7,p0,p2,p1,p6];

val c31 = [p1,p7,p0,p2,p1,p6]; (*duplicates*)
val c32 = [p7,p0,p1,p2,p1,p6];
val c33 = [p7,p0,p2,p1,p6,p1];

val c41 = [p3,p7,p0,p2,p1,p6]; (*illegal_piece*)
val c42 = [p7,p0,p2,p3,p1,p6];
val c43 = [p7,p0,p2,p1,p6,p3];

val c51 = [p1,p7,p0,p2,p1,p6,p4]; (*duplicates and illegal_piece*)
val c52 = [p7,p0,p1,p4,p2,p1,p6];
val c53 = [p4,p7,p0,p2,p1,p6,p1];

val c6 = c30 @ c30 @ c30 @ c30 @ c30 @ c30 @ c30 (*Multiple Duplicates*)

val d as dummy = [((~1,~1,5,true),(~1,~1,5,true),NONE)];

fun f1_t p = legal_piece p = true;
fun f1_f p = legal_piece p = false;
fun f2_t c = legal_config c = true;
fun f2_f c = legal_config c = false;

fun f3_t c p = piece_at c p <>NONE;
fun f3_f c p = piece_at c p = NONE;

val f4 = legal_move_core;

(*Unit tests are of format string*bool where bool is represented by evaluation of a function to an expected value*)
val tests = [
    ("1.00", f1_t p0),
    ("1.01", f1_t p1),
    ("1.02", f1_t p2),
    ("1.03", f1_f p3),
    ("1.04", f1_f p4),
    ("1.05", f1_f p5),
    ("1.06", f1_t p6),
    ("1.07", f1_t p7),
    ("1.08", f1_f p8),
    ("1.09", f1_f p9),

    ("2.00", f2_t c0),
    ("2.01", f2_t c1),
    ("2.02", f2_t c2),
    ("2.03", f2_t c30),

    ("2.10", f2_f c31),
    ("2.11", f2_f c32),
    ("2.12", f2_f c33),
    ("2.20", f2_f c41),
    ("2.21", f2_f c42),
    ("2.22", f2_f c43),
    ("2.30", f2_f c51),
    ("2.31", f2_f c52),
    ("2.32", f2_f c53),
    ("2.40", f2_f c6),

    ("3.00", f3_f c0 q0),  (*Test boundaries*)
    ("3.01", f3_t c1 q0),
    ("3.02", f3_t c2 q0),
    ("3.03", f3_t c2 q7),
    
    ("3.10", f3_t c30 q0), (*Test successful retrievals*)
    ("3.11", f3_t c30 q7),
    ("3.12", f3_t c30 q2),
    ("3.13", f3_t c30 q1),
    ("3.14", f3_t c30 q6),

    ("3.20", f3_f c30 q3), (*Test unsuccessful retrievals*)
    ("3.21", f3_f c2 q3),
    ("3.22", f3_f c1 q3),
    ("3.23", f3_f c0 q3),
    
    (* d is an impossible result so that true can only come about through handling the corresponding exception*)
    ("4.00", f4 c31 p0 (1,1) = d handle IllegalConfig => true),
    ("4.01", f4 c41 p0 (1,1) = d handle IllegalConfig => true),
    ("4.02", f4 c51 p0 (1,1) = d handle IllegalConfig => true),
    ("4.03", f4 c6 p0 (1,1) = d handle IllegalConfig => true),
    ("4.03", f4 [p8] p8 (1,1) = d handle IllegalConfig => true),
    ("4.03", f4 [p9] p9 (1,1) = d handle IllegalConfig => true),

    ("4.10", f4 c1 p0 (1,2) = d handle ImplausibleMove => true),
    ("4.11", f4 c1 p0 (2,1) = d handle ImplausibleMove => true),
    
    ("4.20", f4 [p1] p1 (~1,~1) = d handle IncorrectPlayingDirection => true),
    ("4.21", f4 [p1] p1 (1,~1) = d handle IncorrectPlayingDirection => true),
    ("4.22", f4 [p2] p2 (1,1) = d handle IncorrectPlayingDirection => true),
    ("4.23", f4 [p2] p2 (~1,1) = d handle IncorrectPlayingDirection => true),
    
    ("4.30", f4 [p7] p7 (1,1) = d handle OutOfBoardRange => true),
    ("4.31", f4 [p2] p2 (~3,~3) = d handle OutOfBoardRange => true),
    ("4.32", f4 [p2] p2 (3,~3) = d handle OutOfBoardRange => true),

    ("4.40", f4 [p2,p0] p0 (2,2) = d handle DestinationOccupied => true),

    ("4.50", f4 [] p0 (1,1) = d handle PieceNotPresent => true),
    ("4.51", f4 [p0] p1 (1,1) = d handle PieceNotPresent => true),
    ("4.52", f4 c30 r0 (1,1) = d handle PieceNotPresent => true),

    ("4.60", f4 [p0] p0 (0,0) = d handle ZeroMove => true),
    ("4.61", f4 c30 p1 (0,0) = d handle ZeroMove => true),

    ("4.70", f4 [p0,r1,p2] p0 (3,3) = d handle MultiplePiecesInterspersing => true),
    
    ("4.80", f4 [p0,p1] p0 (2,2) = d handle NotEnemy => true),
    
    ("4.90", f4 [p1] p1 (5,5) = d handle OnlyOneStep => true),
    
    ("4.100", f4 [p1,p2] p1 (5,5) = d handle OnlyTwoStepJump => true),

    (*Different legal moves without capture*)
    ("5.00", f4 [p0] p0 (1,1) = [(p0,(1,1,1,true),NONE)]),                            (*Legal moves on part of King*)
    ("5.01", f4 [p0] p0 (5,5) = [(p0,(5,5,1,true),NONE)]),
    ("5.02", f4 [(5,5,1,true)] (5,5,1,true) (~1,~1) = [((5,5,1,true),(4,4,1,true),NONE)]),
    ("5.03", f4 [(5,5,1,true)] (5,5,1,true) (~5,~5) = [((5,5,1,true),(0,0,1,true),NONE)]),
    ("5.04", f4 [(5,5,1,true)] (5,5,1,true) (~1,1) = [((5,5,1,true),(4,6,1,true),NONE)]),
    ("5.05", f4 [(5,5,1,true)] (5,5,1,true) (1,~1) = [((5,5,1,true),(6,4,1,true),NONE)]),

    ("5.10", f4 [(5,5,1,false)] (5,5,1,false) (~1,1) = [((5,5,1,false),(4,6,1,false),NONE)]),    (*Legal Moves on part of Man 1 and ~1 respectively*)
    ("5.11", f4 [(5,5,1,false)] (5,5,1,false) (1,1) = [((5,5,1,false),(6,6,1,false),NONE)]),
    ("5.12", f4 [(5,5,~1,false)] (5,5,~1,false) (~1,~1) = [((5,5,~1,false),(4,4,~1,false),NONE)]),
    ("5.13", f4 [(5,5,~1,false)] (5,5,~1,false) (1,~1) = [((5,5,~1,false),(6,4,~1,false),NONE)]),

    (*Make different captures*)
    (*In proximity*)
    ("5.20", f4 [(4,4,1,false),(5,5,~1,true)] (4,4,1,false) (2,2) = [((4,4,1,false),(6,6,1,false),SOME (5,5,~1,true))]),
    ("5.21", f4 [(4,4,1,true),(5,5,~1,true)] (4,4,1,true) (2,2) = [((4,4,1,true),(6,6,1,true),SOME (5,5,~1,true))]),
    
    ("5.22", f4 [(4,4,1,false),(5,5,~1,false)] (4,4,1,false) (2,2) = [((4,4,1,false),(6,6,1,false),SOME (5,5,~1,false))]),
    ("5.23", f4 [(4,4,1,true),(5,5,~1,false)] (4,4,1,true) (2,2) = [((4,4,1,true),(6,6,1,true),SOME (5,5,~1,false))]),
    
    ("5.24", f4 [(4,4,1,false),(5,5,~1,true)] (5,5,~1,true) (~2,~2) = [((5,5,~1,true),(3,3,~1,true),SOME (4,4,1,false))]),
    ("5.25", f4 [(4,4,1,false),(5,5,~1,false)] (5,5,~1,false) (~2,~2) = [((5,5,~1,false),(3,3,~1,false),SOME (4,4,1,false))]),

    ("5.26", f4 [(4,4,1,true),(5,5,~1,true)] (5,5,~1,true) (~2,~2) = [((5,5,~1,true),(3,3,~1,true),SOME (4,4,1,true))]),
    ("5.27", f4 [(4,4,1,true),(5,5,~1,false)] (5,5,~1,false) (~2,~2) = [((5,5,~1,false),(3,3,~1,false),SOME (4,4,1,true))]),

    (*Distant Captures*)
    ("5.30", f4 [(1,1,~1,true),(5,5,1,true)] (1,1,~1,true) (5,5) = [((1,1,~1,true),(6,6,~1,true),SOME (5,5,1,true))]),
    ("5.31", f4 [(1,1,~1,true),(5,5,1,true)] (5,5,1,true) (~5,~5) = [((5,5,1,true),(0,0,1,true),SOME (1,1,~1,true))]),

    (*Distant Captures with far landing*)
    ("5.40", f4 [(1,1,~1,true),(5,5,1,true)] (1,1,~1,true) (6,6) = [((1,1,~1,true),(7,7,~1,true),SOME (5,5,1,true))]),
    ("5.41", f4 [(3,3,~1,true),(5,5,1,true)] (5,5,1,true) (~5,~5) = [((5,5,1,true),(0,0,1,true),SOME (3,3,~1,true))]) 

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
  | false => print "--------------SOMETHING IS WRONG-------------------------\n"
