use "johnson_hw5.sml";
val name_hw = "2003 - Autumn - Assignment Five"
 
(*Unit tests are of format string*bool where bool is represented by evaluation of a function to an expected value*)
val tests = [ 
    ("1.00", (next []) > 0 handle InsufficientArguments => true),
    ("1.01", (next [1]) > 1 handle InsufficientArguments => true),

    ("1.10", (next [1,2]) = 3),
    ("1.11", (next [1,2,3]) = 4),
    ("1.12", (next [1,2,3,4]) = 5),
    ("1.13", next [1,3,5] = 7),
    ("1.14", next [0,0] = 0),
    ("1.15", next [0,0,0] = 0),
    ("1.16", next [0,0,0,0] = 0),
    ("1.17", next [0,0,0,0,0] = 0),
    ("1.18", next [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] = 0),
    ("1.19", next [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] = 0),

    ("1.20", (next [1,2,4,8,16,32,64,128,256,512,1024,2048]) = 4096),
    ("1.21", (next [1,5,25,125,625]) = 3125),
    ("1.22", next [~2, 10, ~50] = 250),

    ("1.30", (next [1,2,4,12]) = 7),
    ("1.31", (next [2,2,4,8]) = 6),
    ("1.32", (next [0,2,4,8]) = 8),
    ("1.33",  next [0,10,0,11,0,12,0] = 13),
    
    ("1.40", (next [1,2,3,4,5,6,7,8,9,10,12]) = ~5 handle NothingApplies => true ),
    ("1.41", (next [0,2,3,4,5,6,7,8,9,10,11]) = ~5 handle NothingApplies => true ),
    ("1.42", (next [1,2,3,4,5,6,6,7,8,9,10,11]) = ~5 handle NothingApplies => true ),
    
    ("1.50", (next [1,2,4,5,8]) = ~5 handle NothingApplies => true ),
    
    ("2.00", gen((fn(n) => 7 + n*2),10) = [7,9,11,13,15,17,19,21,23,25])
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


