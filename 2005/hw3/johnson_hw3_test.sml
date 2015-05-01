use "johnson_hw3.sml";
val name_hw = "2005 - Assignment Three"

val f1 = lineop_func
val f2 = lineprog_func
val f3 = pushn_right_safe
val f4 = pushn_right_block

val a_pair = ([(1,2),(6,7),(3,4),(8,9)],[(3,4),(6,7),(1,2),(8,9)])
val a_6pair = ([(1,2),(2,3),(4,5),(6,7),(3,4),(8,9)],[(13,14),(16,17),(11,12),(18,19),(21,22),(23,24)])
val a_100pair = ([(1,001),(1,002),(1,003),(1,004),(1,005),(1,006),(1,007),(1,008),(1,009),(1,010),(1,011),(1,012),(1,013),(1,014),(1,015),(1,016),(1,017),(1,018),(1,019),(1,020),(1,021),(1,022),(1,023),(1,024),(1,025),(1,026),(1,027),(1,028),(1,029),(1,030),(1,031),(1,032),(1,033),(1,034),(1,035),(1,036),(1,037),(1,038),(1,039),(1,040),(1,041),(1,042),(1,043),(1,044),(1,045),(1,046),(1,047),(1,048),(1,049),(1,050),(1,051),(1,052),(1,053),(1,054),(1,055),(1,056),(1,057),(1,058),(1,059),(1,060),(1,061),(1,062),(1,063),(1,064),(1,065),(1,066),(1,067),(1,068),(1,069),(1,070),(1,071),(1,072),(1,073),(1,074),(1,075),(1,076),(1,077),(1,078),(1,079),(1,080),(1,081),(1,082),(1,083),(1,084),(1,085),(1,086),(1,087),(1,088),(1,089),(1,090),(1,091),(1,092),(1,093),(1,094),(1,095),(1,096),(1,097),(1,098),(1,099),(1,100)],[(1,001),(1,002),(1,003),(1,004),(1,005),(1,006),(1,007),(1,008),(1,009),(1,010),(1,011),(1,012),(1,013),(1,014),(1,015),(1,016),(1,017),(1,018),(1,019),(1,020),(1,021),(1,022),(1,023),(1,024),(1,025),(1,026),(1,027),(1,028),(1,029),(1,030),(1,031),(1,032),(1,033),(1,034),(1,035),(1,036),(1,037),(1,038),(1,039),(1,040),(1,041),(1,042),(1,043),(1,044),(1,045),(1,046),(1,047),(1,048),(1,049),(1,050),(1,051),(1,052),(1,053),(1,054),(1,055),(1,056),(1,057),(1,058),(1,059),(1,060),(1,061),(1,062),(1,063),(1,064),(1,065),(1,066),(1,067),(1,068),(1,069),(1,070),(1,071),(1,072),(1,073),(1,074),(1,075),(1,076),(1,077),(1,078),(1,079),(1,080),(1,081),(1,082),(1,083),(1,084),(1,085),(1,086),(1,087),(1,088),(1,089),(1,090),(1,091),(1,092),(1,093),(1,094),(1,095),(1,096),(1,097),(1,098),(1,099),(1,100)])
		    
val a_3prog = [PushRight,SwapFirst,PushRight,SwapFirst,
	       PushLeft,SwapFirst,
	       PushRight,PushRight]
val a_4prog = [PushRight, SwapFirst, PushRight, SwapFirst, PushRight, SwapFirst, 
	       PushLeft, SwapFirst, PushLeft, SwapFirst, 
	       PushRight, SwapFirst,
	       PushRight, PushRight]
		  
fun compare lop1 lop2 =
  case (lop1,lop2) of
      (PushRight,PushRight) => true
    | (PushLeft,PushLeft) => true
    | (SwapFirst,SwapFirst) => true
    | (ApplyRight _, ApplyRight _) => true
    | (ApplyLeft _, ApplyLeft _) => true
    | _ => false
fun comparelists(l1, l2) = List.foldl(fn((x,y),acc) => compare x y) true (ListPair.zip(l1,l2))

fun compare_pairs(p1,(p2l,p2r),n) =
  let val left = List.drop(p2r,n)
      val right = List.take(p2r,n)@p2l
  in p1 = (left,right)
  end
 
(*Unit tests are of format string*bool where bool is represented by evaluation of a function to an expected value*)
val tests = [
    ("1.00", f1 PushRight ([],[]) <> ([],[]) handle Empty => true),
    ("1.01", f1 PushLeft ([],[]) <> ([],[]) handle Empty => true),
    ("1.02", f1 PushRight ([],[(1,2)]) <> ([],[]) handle Empty => true),
    ("1.03", f1 PushLeft ([(1,2)],[]) <> ([],[]) handle Empty => true),
    ("1.04", f1 SwapFirst ([],[]) <> ([],[]) handle Empty => true),
    ("1.05", f1 SwapFirst ([(1,2)],[]) <> ([],[]) handle Empty => true),
    ("1.06", f1 SwapFirst ([],[(1,2)]) <> ([],[]) handle Empty => true),
    ("1.07", f1 (ApplyRight(fn(x) => x)) ([],[]) <> ([],[]) handle Empty => true),
    ("1.08", f1 (ApplyLeft (fn(x) => x)) ([],[]) <> ([],[]) handle Empty => true),
    ("1.09", f1 (ApplyRight(fn(x) => x)) ([(1,2)],[]) <> ([],[]) handle Empty => true),
    ("1.10", f1 (ApplyLeft (fn(x) => x)) ([],[(1,2)]) <> ([],[]) handle Empty => true),

    ("1.20", f1 PushRight ([(1,2)],[]) = ([],[(1,2)])),
    ("1.21", f1 PushRight ([(1,2),(2,4)],[]) = ([(2,4)],[(1,2)])),
    ("1.22", f1 PushLeft ([],[(1,2)]) = ([(1,2)],[])),
    ("1.23", f1 PushLeft ([],[(1,2),(2,4)]) = ([(1,2)],[(2,4)])),
    ("1.24", f1 PushRight ([(1,2)],[]) = ([],[(1,2)])),

    ("1.30", f1 SwapFirst ([(1,2)],[(3,4)]) = ([(3,4)],[(1,2)])),
    ("1.31", f1 SwapFirst ([(1,2),(6,7)],[(3,4),(8,9)]) = ([(3,4),(6,7)],[(1,2),(8,9)])),
    
    ("1.40", f1 (ApplyRight(fn(x) => x)) ([],[(1,1)]) = ([],[(1,1)])),
    ("1.41", f1 (ApplyLeft(fn(x) => x)) ([(1,1)],[]) = ([(1,1)],[])),
    ("1.42", f1 (ApplyRight(fn(x) => x)) ([(2,2)],[(1,1)]) = ([(2,2)],[(1,1)])),
    ("1.43", f1 (ApplyLeft(fn(x) => x)) ([(2,2)],[(1,1)]) = ([(2,2)],[(1,1)])),
    ("1.44", f1 (ApplyRight(fn(x) => x)) ([(2,2)],[(1,1),(3,4)]) = ([(2,2)],[(1,1),(3,4)])),
    ("1.45", f1 (ApplyLeft(fn(x) => x)) ([(2,2),(4,5)],[(1,1)]) = ([(2,2),(4,5)],[(1,1)])),
    ("1.46", f1 (ApplyRight(fn(x,y) => (y,x))) ([(2,2)],[(1,2),(3,4)]) = ([(2,2)],[(2,1),(3,4)])),
    ("1.47", f1 (ApplyLeft(fn(x,y) => (y,x))) ([(2,3),(4,5)],[(1,1)]) = ([(3,2),(4,5)],[(1,1)])),
    ("1.48", f1 (ApplyRight(fn(x,y) => (y,x*4))) ([(2,2)],[(1,2),(3,4)]) = ([(2,2)],[(2,4),(3,4)])),
    ("1.49", f1 (ApplyLeft(fn(x,y) => (y,x*4))) ([(2,3),(4,5)],[(1,1)]) = ([(3,8),(4,5)],[(1,1)])),
    ("2.00", f2 [PushRight] ([(1,1)],[]) = ([],[(1,1)])),
    ("2.01", f2 [PushRight,PushLeft,PushRight] ([(1,1)],[]) = ([],[(1,1)])),
    ("2.02", f2 [SwapFirst,PushRight] ([(1,1)],[(2,2)]) = ([],[(2,2),(1,1)])),
    ("2.03", f2 [PushRight,PushRight,SwapFirst, ApplyRight(fn(x,y)=>(y,x)), ApplyLeft(fn(x,y)=>(2*y,3*x))] ([(1,2),(6,7),(3,4),(8,9)],[(3,4),(6,7),(1,2),(8,9)]) = ([(14,18),(8,9)],[(4,3),(1,2),(3,4),(6,7),(1,2),(8,9)])),

    ("3.00", comparelists(f3 0, [])),
    ("3.01", comparelists(f3 ~1, [])),
    ("3.02", comparelists(f3 1, [PushRight, ApplyRight(fn(x,y)=>(y,x))])),
    ("3.03", comparelists(f3 5, [PushRight, ApplyRight(fn(x,y)=>(y,x)),PushRight, ApplyRight(fn(x,y)=>(y,x)),PushRight, ApplyRight(fn(x,y)=>(y,x)),PushRight, ApplyRight(fn(x,y)=>(y,x)),PushRight, ApplyRight(fn(x,y)=>(y,x))])),

    ("4.00", comparelists((f4 0),[])),
    ("4.01", comparelists((f4 ~5),[])),
    ("4.02", comparelists((f4 1),[PushRight])),
    ("4.03", comparelists((f4 2),[PushRight,SwapFirst]@(f4 1))),
    ("4.04", comparelists((f4 3),a_3prog@(f4 2))),
    ("4.05", comparelists((f4 4),a_4prog@(f4 3))),
    
    ("4.10", compare_pairs(f2 (f4 0) a_100pair, a_100pair,0)),
    ("4.10", compare_pairs(f2 (f4 1) a_100pair, a_100pair,1)),
    ("4.10", compare_pairs(f2 (f4 2) a_100pair, a_100pair,2)),
    ("4.10", compare_pairs(f2 (f4 3) a_100pair, a_100pair,3)),
    ("4.10", compare_pairs(f2 (f4 4) a_100pair, a_100pair,4)),
    ("4.10", compare_pairs(f2 (f4 5) a_100pair, a_100pair,5)),
    ("4.10", compare_pairs(f2 (f4 95) a_100pair, a_100pair,95)),
    ("4.10", compare_pairs(f2 (f4 96) a_100pair, a_100pair,96)),
    ("4.10", compare_pairs(f2 (f4 97) a_100pair, a_100pair,97)),
    ("4.10", compare_pairs(f2 (f4 98) a_100pair, a_100pair,98)),
    ("4.10", compare_pairs(f2 (f4 99) a_100pair, a_100pair,99)),
    ("4.10", compare_pairs(f2 (f4 100) a_100pair, a_100pair,100))
];

print ("\n----------------------"^name_hw^"--------------------------\n"); (*Name display to assert correct test file is running*)
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
    true => print ((Int.toString (List.length(tests))) ^ "--TESTS RUN------------EVERY TESTS PASSED-------------\n")
		  
  | false => print "--------------SOMETHING IS WRONG-------------------------\n";
