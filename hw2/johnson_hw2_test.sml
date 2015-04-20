(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Tests *)
fun run_test(l) =
  case l of
      [] => true
    | (_,x,y)::xs' => x = y andalso run_test(xs')

fun retrieve_failed_tests(l) =
  case l of
      [] => []
    | (id,x,y)::xs' => if x <> y
		       then ("***Failed*** " ^ id,x,y) :: retrieve_failed_tests(xs')
		       else retrieve_failed_tests(xs') 
					    
val s1 = "CIvuNCJacBmzmN"
val s2 = "xJeC3Oe6nJV4Cg"
val s3 = "7IK3QAz2kNRCLs"
val s4 = "i2oqTL6VTvJLUy"
val s5 = "PMA3UfR5oYARjA"
val s6 = "qoA5sTMLzuDRmb"
val s7 = "MTmxbwXCSt5YLB"
val s8 = "pp7htnOE3ag9Lq"
val s9 = "urpWliQU08Gnk8"
val s10 = "He1lIb6X0iu8IX"

val sl1 = [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10]
val sl2 = ""::sl1
val sl3 = []
val sl4 = [""]
val sl5 = ["1"];
val sl6 = [s1,s2,s3,s4,s1,s5,s6,s7,s8,s9,s10,s1];

val testcases_all_except_options = [
    ("0", all_except_option("", sl2), SOME sl1),
    ("1", all_except_option(s1,sl1), SOME (tl sl1)),
    ("2", all_except_option(s1,sl2), SOME (""::(tl sl1))),
    ("3", all_except_option(s2,sl3), NONE),
    ("4", all_except_option("",sl1), SOME sl1),
    ("5", all_except_option("",sl4), SOME []),
    ("6", all_except_option("random",sl4), SOME sl4),
    ("7", all_except_option("1",sl5), SOME []),
    ("8", all_except_option(s6,sl1), SOME [s1,s2,s3,s4,s5,s7,s8,s9,s10]),
    ("9", all_except_option(s10,sl1), SOME [s1,s2,s3,s4,s5,s6,s7,s8,s9]),
    ("10", all_except_option(s1,sl6), SOME (tl sl1))];

if run_test(testcases_all_except_options)
then [("ALL TESTS PASSED FOR fun all_except_options",NONE,NONE)]
else retrieve_failed_tests(testcases_all_except_options);
			  
(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end
