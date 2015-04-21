(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Tests *)
(* Unit testing with the format: (id: string, function, expected_result) *)

fun sort_list(l) =
  let fun get_max(l') =
	case l' of
	    [] => ""
	 | x::xs' => let val max = get_max(xs')
		    in if x > max
		       then x
		       else max
		    end
  in
  case l of
      [] => []
    | x::xs' => if x = get_max(l)
		then x::sort_list(xs')
		else sort_list(xs'@[x])
  end

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
    ("4", all_except_option("",sl1), NONE),
    ("5", all_except_option("",sl4), SOME []),
    ("6", all_except_option("random",sl4), NONE),
    ("7", all_except_option("1",sl5), SOME []),
    ("8", all_except_option(s6,sl1), SOME [s1,s2,s3,s4,s5,s7,s8,s9,s10]),
    ("9", all_except_option(s10,sl1), SOME [s1,s2,s3,s4,s5,s6,s7,s8,s9]),
    ("10", all_except_option(s1,sl6), SOME (tl sl1))];

fun testcases_get_substitutions1(g,h,f) =
[
    ("0", g(h([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")), f(["Fredrick","Freddie","F"])),
    ("1", g(h([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")), f(["Jeffrey","Geoff","Jeffrey"])),
    ("2",g(h([["Zella","Tomika","Zella"],["Charlyne","Nenita","Tatiana"],["Charlyne","Kandi","Tilda"],["Tomika","Lauren","Kandi","Tatiana"]], "Zella")), f(["Tomika"])),
    ("3",g(h([["Nora","Maricruz","Tatiana","Maricruz","Carylon","Zella"],["Zella","Theresia","Carylon"],["Zella","Marlo","Maricruz"],["Zella","Kandi","Kandi","Newton","Tatiana","Gretta","Tomas"],["Tomika","Tilda","Zella","Nenita","Tilda"]], "Zella")), f(["Nora","Maricruz","Tatiana","Maricruz","Carylon","Theresia","Carylon","Marlo","Maricruz","Kandi","Kandi","Newton","Tatiana","Gretta","Tomas","Tomika","Tilda","Nenita","Tilda"])),
    ("4",g(h([["Tomika","Corene","Marlo","Nenita"],["Kandi","Marlo","Tomika","Tomas","Neva","Kandi"],["Nora","Gretta","Kandi","Kandi","Zella","Maricruz"],["Tomika","Lauren","Kandi","Tatiana"],["Tomika","Marlo","Kandi","Corene"]], "Tomika")), f(["Corene","Marlo","Nenita","Kandi","Marlo","Tomas","Neva","Kandi","Lauren","Kandi","Tatiana","Marlo","Kandi","Corene"])),
    ("5",g(h([["Nora","Gretta","Kandi","Kandi","Zella","Maricruz"],["Zella","Theresia","Carylon"],["Kandi","Corene","Tatiana","Zella","Kandi","Bree"],["Tomika","Tilda","Nenita","Tilda"],["Charlyne","Nenita","Tatiana"]], "Nora")), f(["Gretta","Kandi","Kandi","Zella","Maricruz"])),
    ("6",g(h([["Zella","Lauren","Tomika"],["Zella","Tomika","Zella"],["Nora","Tilda","Kandi","Carylon","Nora","Dwana"],["Lauren","Vince","Tilda","Nenita"],["Zella","Marlo","Maricruz"]], "Maricruz")), f(["Zella","Marlo"])),
    ("7",g(h([["Kandi","Marlo","Tomika","Tomas","Neva","Kandi"],["Tomika","Corene","Marlo","Nenita"],["Zella","Lauren","Tomika"],["Nora","Gretta","Kandi","Kandi","Zella","Maricruz"],["Tomika","Tilda","Nenita","Tilda"]], "Corene")), f(["Tomika","Marlo","Nenita"])),
    ("8",g(h([["Lauren","Bree","Tomika","Bree"],["Nora","Gretta","Kandi","Kandi","Zella","Maricruz"],["Charlyne","Kandi","Tilda"],["Nora","Gretta","Kandi","Kandi","Zella","Maricruz"],["Lauren","Neva","Maricruz","Corazon"]], "NOMATCH")), f([])),
    ("9",g(h([["Kandi","Gretta","Tatiana","Bree","Tatiana","Tilda"],["Tomika","Marlo","Kandi","Corene"],["Tomika","Corene","Marlo","Nenita"],["Tomika","Corene","Marlo","Nenita"],["Lauren","Tomas","Lauren","Bree"]], "Tomika")), f(["Marlo","Kandi","Corene","Corene","Marlo","Nenita","Corene","Marlo","Nenita"])),
    ("10",g(h([["Zella","Marlo","Maricruz","Bree"],["Charlyne","Bree","Bree"],["Kandi","Corene","Tatiana","Zella","Kandi","Bree"],["Zella","Theresia","Carylon","Bree"]], "Bree")), f(["Zella","Marlo","Maricruz","Charlyne","Kandi","Corene","Tatiana","Zella","Kandi","Zella","Theresia","Carylon"])),
    ("11",g(h([[]], "Frank")), f([])),
    ("12",g(h([[""]], "Frank")), f([])),
    ("13",g(h([["","Tomika","Tilda","Nenita","Tilda"],["Nora","Maricruz","Tatiana","Maricruz","Carylon","Zella"],["Charlyne","Kandi","Tilda"],["Tomika","Newton","Bree","Lauren"],["Charlyne","Lauren","Kandi"]], "")), f(["Tomika","Tilda","Nenita","Tilda"]))

    ];
    

if run_test(testcases_all_except_options)
then [("ALL TESTS PASSED FOR fun all_except_options",NONE,NONE)]
else retrieve_failed_tests(testcases_all_except_options);

if run_test(testcases_get_substitutions1((fn(x)=>x),get_substitutions1, (fn(x)=>x)))
then [("ALL TESTS PASSED FOR fun get_substitutions1",[],[])]
else retrieve_failed_tests(testcases_get_substitutions1((fn(x)=>x),get_substitutions1, (fn(x)=>x)));

if run_test(testcases_get_substitutions1(sort_list,get_substitutions2, sort_list))
then [("ALL TESTS PASSED FOR fun get_substitutions2",[],[])]
else retrieve_failed_tests(testcases_get_substitutions1(sort_list,get_substitutions2, sort_list));

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [
    {first="Fred", last="Smith", middle="W"},{first="Fredrick", last="Smith", middle="W"},{first="Freddie", last="Smith", middle="W"},{first="F", last="Smith", middle="W"}];

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
