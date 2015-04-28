use "johnson_hw.sml";

val f1 = find_playable
val f2 = without_bone
val f3 = layout_summary
val f4 = best_move
	     
(*Unit tests are of format string*bool where bool is represented by evaluation of a function to an expected value*)
val tests = [
    ("1.0", f1([], 3) = NONE),
    ("1.1", f1([(1,1)], 1) = SOME (1,1)),
    ("1.2", f1([(1,1)], 2) = NONE),
    ("1.3", f1([(1,1),(2,1),(3,4)], 1) = SOME (1,1)),
    ("1.4", f1([(1,1),(2,1),(3,4)], 2) = SOME (2,1)),
    ("1.5", f1([(1,1),(2,1),(3,4)], 3) = SOME (3,4)),
    ("1.6", f1([(1,1),(2,1),(3,4)], 4) = SOME (3,4)),
    ("1.7", f1([(1,1),(2,1),(3,4)], 5) = NONE),

    ("2.0", f2([],(1,3)) = []),
    ("2.1", f2([(2,3)],(2,3)) = [] andalso f2([(3,2)],(2,3)) = [] andalso f2([(2,3)],(3,2)) = []),
    ("2.2", f2([(5,6),(3,4),(1,2)],(5,6)) = [(3,4),(1,2)] andalso f2([(5,6),(3,4),(1,2)],(3,4)) = [(5,6),(1,2)] andalso f2([(5,6),(3,4),(1,2)],(1,2)) = [(5,6),(3,4)]),

    ("2.3", f2([(5,6),(3,4),(1,2),(5,6)],(5,6)) = [(3,4),(1,2),(5,6)] andalso f2([(5,6),(3,4),(3,4),(1,2)],(3,4)) = [(5,6),(3,4),(1,2)] andalso f2([(5,6),(1,2),(3,4),(1,2)],(1,2)) = [(5,6),(3,4),(1,2)]),

    ("3.0", f3([]) = NONE),
    ("3.1", f3([(1,7)]) = SOME (1,7)),
    ("3.2", f3([(1,1),(2,1)]) = SOME (1,1)),
    ("3.3", f3([(1,1),(2,1),(3,4)]) = SOME (1,4)),
    ("3.4", f3([(5,6),(3,4),(1,2),(5,6)]) = SOME (5,6)),

    ("4.0", f4([],[]) = PassDraw),
    ("4.1", f4([],[(1,2)]) = PlayFirst (1,2)),
    ("4.2", f4([(1,2)],[(1,2)]) = PlayLeft (1,2)),
    ("4.3", f4([(2,1)],[(1,2)]) = PlayLeft (1,2)),
    ("4.4", f4([(3,4),(4,6)],[(3,2)]) = PlayLeft (3,2)),
    ("4.5", f4([(3,4),(4,6)],[(1,3)]) = PlayLeft (1,3)),
    ("4.6", f4([(3,4),(4,6)],[(3,6)]) = PlayRight (3,6)),
    ("4.7", f4([(3,4),(4,6)],[(6,3)]) = PlayRight (6,3)),
    ("4.8", f2([(5,6),(3,4),(1,2),(3,4)],[(7,7)]) = PassDraw)
    
];

print "\n------------------------------------------------\n";
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
