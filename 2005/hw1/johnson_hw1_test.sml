use "johnson_hw1.sml";
val f1 = legal_bone
val f2 = compatible_bones
val f3 = all_legal_bones
val f4 = no_doubles
val f5 = make_suit
val f6 = all_same_suit
	     
val tests = [

    ("1", f1(1,2)  andalso f1(1,1) andalso f1(max,max) andalso not (f1(0,2)) andalso not (f1(2,0)) andalso not (f1(~1,3)) andalso not (f1(max + 1,4)) andalso not (f1(5,max + 1)) = true),

    ("2", f2((3,4),(3,4)) andalso f2((1,5),(1,6)) andalso f2((2,3),(1,3)) andalso f2((3,4),(2,3)) andalso f2((4,5),(5,6)) andalso not(f2((2,3),(4,5))) andalso f2((max + 100, 1),(1, max + 20)) = true),

    ("3.0", f3([]) = true),
    ("3.1", f3([(1,1)]) = true),
    ("3.2", f3([(max,max)]) = true),
    ("3.3", f3([(1,1),(1,2),(1,3),(7,1),(7,6),(7,7)]) = true),
    ("3.4", f3([(0,1),(1,1),(1,2),(1,3),(7,1),(7,6),(7,7)]) = false),
    ("3.5", f3([(1,0),(1,2),(1,3),(7,1),(7,6),(7,7)]) = false),
    ("3.6", f3([(1,1),(1,2),(1,3),(7,1),(7,6),(7,7),(1,~7)]) = false),
    ("3.7", f3([(1,1),(1,2),(1,3),(7,1),(7,6),(7,7),(~3,6)]) = false),
    ("3.8", f3([(1,1),(1,2),(1,3),(max + 1,2),(7,1),(7,6),(7,7)]) = false),
    ("3.9", f3([(1,1),(1,2),(1,3),(3, max + 10),(7,1),(7,6),(7,7)]) = false),

    ("4.0", f4([]) = true),
    ("4.1", f4([(1,2)]) = true),
    ("4.2", f4([(3,3)]) = false),
    ("4.3", f4([(2,3),(3,4),(4,5),(7,8),(max + 3,4),(~1,~max)]) = true),
    ("4.4", f4([(1,1),(2,3),(3,4),(4,5),(7,8),(max + 3,4),(~1,~max)]) = false),
    ("4.5", f4([(2,3),(3,4),(4,5),(7,8),(max + 3,4),(~1,~max),(2,2)]) = false),
    ("4.6", f4([(2,3),(3,4),(4,5),(max,max),(7,8),(max + 3,4),(~1,~max)]) = false),
    ("4.7", f4([(2,3),(3,4),(4,5),(7,8),(3,3),(max + 3,4),(~1,~max)]) = false),

    ("5.0", f5(0) = []),
    ("5.1", f5(1) = [(1,1)]),
    ("5.2", f5(2) = [(2,2),(1,2),(2,1)]),
    ("5.3", f5(3) = [(3,3),(2,3),(3,2),(1,3),(3,1)]),

    ("6.0", f6([]) = false),
    ("6.1", f6([(2,3)]) = true),
    ("6.2", f6([(2,2),(1,2),(2,1)]) = true),
    ("6.3", f6([(3,3),(2,3),(3,2),(1,3),(3,1)]) = true),
    ("6.4", f6([(1,1),(3,3),(2,3),(3,2),(1,3),(3,1)]) = false),
    ("6.5", f6([(3,3),(2,3),(5,7),(3,2),(1,3),(3,1)]) = false),
    ("6.6", f6([(3,3),(2,3),(3,2),(1,3),(3,1),(4,5)]) = false)
    
];

print "\n------------------------------------------------\n";
fun all_tests(tests) =
	let fun helper(tests: (string*bool) list, all_passed) = 
		case tests of
			[] => all_passed
	  	  | (st, true)::rest =>  helper(rest, all_passed)
	  	  | (st, false)::rest => (print ("***" ^ st ^" !!!FAILED!!!\n"); helper(rest, false))
	in
		helper(tests, true)
	end;

case all_tests(tests) of
    true => print "--------------EVERY TESTS PASSED-------------\n"
  | false => print "--------------SOMETHING IS WRONG-------------------------\n";
