use "johnson_hw.sml";

val f1 = find_playable

(*Unit tests are of format string*bool where bool is represented by evaluation of a function to an expected value*)
val tests = [
    ("1.0", f1([], 3) = NONE),
    ("1.1", f1([(1,1)], 1) = SOME (1,1)),
    ("1.2", f1([(1,1)], 2) = NONE),
    ("1.3", f1([(1,1),(2,1),(3,4)], 1) = SOME (1,1)),
    ("1.4", f1([(1,1),(2,1),(3,4)], 2) = SOME (2,1)),
    ("1.5", f1([(1,1),(2,1),(3,4)], 3) = SOME (3,4)),
    ("1.6", f1([(1,1),(2,1),(3,4)], 4) = SOME (3,4)),
    ("1.7", f1([(1,1),(2,1),(3,4)], 5) = NONE)
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
