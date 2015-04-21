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

val number = {
Spades=[
(Spades,Num(10)),
(Spades,Num(2)),
(Spades,Num(3)),
(Spades,Num(4)),
(Spades,Num(5)),
(Spades,Num(6)),
(Spades,Num(7)),
(Spades,Num(8)),
(Spades,Num(9))],
Clubs=[
(Clubs,Num(10)),
(Clubs,Num(2)),
(Clubs,Num(3)),
(Clubs,Num(4)),
(Clubs,Num(5)),
(Clubs,Num(6)),
(Clubs,Num(7)),
(Clubs,Num(8)),
(Clubs,Num(9))],
Hearts=[
(Hearts,Num(10)),
(Hearts,Num(2)),
(Hearts,Num(3)),
(Hearts,Num(4)),
(Hearts,Num(5)),
(Hearts,Num(6)),
(Hearts,Num(7)),
(Hearts,Num(8)),
(Hearts,Num(9))],
Diamonds=[
(Diamonds,Num(10)),
(Diamonds,Num(2)),
(Diamonds,Num(3)),
(Diamonds,Num(4)),
(Diamonds,Num(5)),
(Diamonds,Num(6)),
(Diamonds,Num(7)),
(Diamonds,Num(8)),
(Diamonds,Num(9))]
};

val honors ={
Spades=[
(Spades,Queen),
(Spades,Ace),
(Spades,Jack),
(Spades,King)],

Diamonds=[
(Diamonds,Queen),
(Diamonds,Ace),
(Diamonds,Jack),
(Diamonds,King)],

Hearts=[
(Hearts,Queen),
(Hearts,Ace),
(Hearts,Jack),
(Hearts,King)],

Clubs=[
(Clubs,Queen),
(Clubs,Ace),
(Clubs,Jack),
(Clubs,King)]
};

val shuffled = [
(Spades,Num(5)),
(Hearts,Num(7)),
(Diamonds,Num(4)),
(Diamonds,Num(6)),
(Hearts,Ace),
(Clubs,Queen),
(Hearts,Num(5)),
(Hearts,Queen),
(Clubs,Jack),
(Hearts,King),
(Spades,Num(4)),
(Diamonds,King),
(Spades,Ace),
(Hearts,Num(8)),
(Hearts,Num(3)),
(Hearts,Num(6)),
(Hearts,Jack),
(Spades,Num(3)),
(Diamonds,Num(5)),
(Clubs,Num(4)),
(Spades,Num(8)),
(Spades,Num(6)),
(Hearts,Num(4)),
(Diamonds,Ace),
(Clubs,Num(3)),
(Diamonds,Num(7)),
(Hearts,Num(9)),
(Spades,Num(9)),
(Clubs,King),
(Spades,Queen),
(Clubs,Num(9)),
(Clubs,Num(10)),
(Diamonds,Num(9)),
(Clubs,Num(7)),
(Diamonds,Queen),
(Hearts,Num(10)),
(Diamonds,Num(10)),
(Diamonds,Num(2)),
(Clubs,Ace),
(Clubs,Num(8)),
(Spades,Jack),
(Hearts,Num(2)),
(Clubs,Num(5)),
(Spades,Num(10)),
(Spades,Num(2)),
(Spades,Num(7)),
(Diamonds,Num(8)),
(Clubs,Num(2)),
(Diamonds,Jack),
(Diamonds,Num(3)),
(Spades,King),
(Clubs,Num(6))
];

val deck = [
(Spades,Num(10)),
(Spades,Num(2)),
(Spades,Num(3)),
(Spades,Num(4)),
(Spades,Num(5)),
(Spades,Num(6)),
(Spades,Num(7)),
(Spades,Num(8)),
(Spades,Num(9)),
(Clubs,Num(10)),
(Clubs,Num(2)),
(Clubs,Num(3)),
(Clubs,Num(4)),
(Clubs,Num(5)),
(Clubs,Num(6)),
(Clubs,Num(7)),
(Clubs,Num(8)),
(Clubs,Num(9)),
(Hearts,Num(10)),
(Hearts,Num(2)),
(Hearts,Num(3)),
(Hearts,Num(4)),
(Hearts,Num(5)),
(Hearts,Num(6)),
(Hearts,Num(7)),
(Hearts,Num(8)),
(Hearts,Num(9)),
(Diamonds,Num(10)),
(Diamonds,Num(2)),
(Diamonds,Num(3)),
(Diamonds,Num(4)),
(Diamonds,Num(5)),
(Diamonds,Num(6)),
(Diamonds,Num(7)),
(Diamonds,Num(8)),
(Diamonds,Num(9)),
(Spades,Queen),
(Spades,Ace),
(Spades,Jack),
(Spades,King),
(Diamonds,Queen),
(Diamonds,Ace),
(Diamonds,Jack),
(Diamonds,King),
(Hearts,Queen),
(Hearts,Ace),
(Hearts,Jack),
(Hearts,King),
(Clubs,Queen),
(Clubs,Ace),
(Clubs,Jack),
(Clubs,King)
];

val lc1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Jack),(Spades,Num(8))];
val lc2 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Hearts,King),(Spades,Ace),(Clubs,Jack),(Spades,Num(8))];
fun testcases_all_same_colors(f) = [
    ("1",f(lc1), true),
    ("2",f(#Spades number), true),
    ("3",f(#Clubs number), true),
    ("4",f(#Diamonds number), true),
    ("5",f(#Hearts number), true),
    ("6",f(#Spades honors), true),
    ("7",f(#Clubs honors), true),
    ("8",f(#Diamonds honors), true),
    ("9",f(#Hearts honors), true),
    
    ("10",f((Clubs,Queen)::(#Spades number)), true),
    ("11",f((Spades,Queen)::(#Clubs number)), true),
    ("12",f((Hearts,King)::(#Diamonds number)), true),
    ("13",f((Diamonds,King)::(#Hearts number)), true),
    
    ("14",f((Diamonds,King)::(#Spades honors)), false),
    ("15",f((Hearts,King)::(#Clubs honors)), false),
    ("16",f((Spades,Queen)::(#Diamonds honors)), false),
    ("17",f((Clubs,Queen)::(#Hearts honors)), false),
    ("18",f((#Spades number)@[(Diamonds,King)]), false),
    ("19",f((#Clubs number)@[(Hearts,King)]), false),
    ("20",f((#Diamonds number)@[(Spades,Queen)]), false),
    ("21",f((#Hearts number)@[(Clubs,Queen)]), false),
    ("22",f(lc2), false),
    ("23",f(shuffled), false),
    ("24",f(deck), false)
];

fun testcases_sum_cards(f) = [
    ("1",f(lc1), 62),
    ("2",f(#Spades number), 54),
    ("3",f(#Clubs number), 54),
    ("4",f(#Diamonds number), 54),
    ("5",f(#Hearts number), 54),
    ("6",f(#Spades honors), 41),
    ("7",f(#Clubs honors), 41),
    ("8",f(#Diamonds honors), 41),
    ("9",f(#Hearts honors), 41),
    
    ("10",f((Clubs,Queen)::(#Spades number)), 64),
    ("11",f((Spades,Queen)::(#Clubs number)), 64),
    ("12",f((Hearts,King)::(#Diamonds number)), 64),
    ("13",f((Diamonds,King)::(#Hearts number)), 64),
    
    ("14",f((Diamonds,King)::(#Spades honors)), 51),
    ("15",f((Hearts,King)::(#Clubs honors)), 51),
    ("16",f((Spades,Queen)::(#Diamonds honors)), 51),
    ("17",f((Clubs,Queen)::(#Hearts honors)), 51),
    
    ("18",f((#Spades number)@[(Diamonds,King)]), 64),
    ("19",f((#Clubs number)@[(Hearts,King)]), 64),
    ("20",f((#Diamonds number)@[(Spades,Queen)]), 64),
    ("21",f((#Hearts number)@[(Clubs,Queen)]), 64),
    ("22",f(lc2), 72),
    ("23",f(shuffled), 380),
    ("24",f(deck), 380),
    ("25",f([]), 0),
    ("26",f([(Clubs, Ace)]), 11),
    ("27",f([(Clubs, Num(~10))]), ~10)
];

fun test_cases_score(f) = [
    ("1",f(lc1, 62), 0),
    ("2",f(#Spades number,60), 3),
    ("3",f(#Clubs number,50),6),
    ("4",f(#Diamonds number,54),0),
    ("5",f(#Hearts number,80), 13),
    ("6",f(#Spades honors,41), 0),
    ("7",f(#Clubs honors,42), 0),
    ("8",f(#Diamonds honors,40), 1),
    ("9",f(#Hearts honors,43),1),
    
    ("10",f((Clubs,Queen)::(#Spades number),1),94),
    ("11",f((Spades,Queen)::(#Clubs number),100), 18),
    ("12",f((Hearts,King)::(#Diamonds number),64), 0),
    ("13",f((Diamonds,King)::(#Hearts number),63), 1),
    
    ("14",f((Diamonds,King)::(#Spades honors),52), 1),
    ("15",f((Hearts,King)::(#Clubs honors),50), 3),
    ("16",f((Spades,Queen)::(#Diamonds honors),41), 30),
    ("17",f((Clubs,Queen)::(#Hearts honors),60), 9),
    
    ("18",f((#Spades number)@[(Diamonds,King)],64), 0),
    ("19",f((#Clubs number)@[(Hearts,King)],6), 174),
    ("20",f((#Diamonds number)@[(Spades,Queen)],24), 120),
    ("21",f((#Hearts number)@[(Clubs,Queen)],664), 600),
    ("22",f(lc2,50), 66),
    ("23",f(shuffled,400), 20),
    ("24",f(deck,360), 60),
    ("25",f([],10), 5),
    ("26",f([(Clubs, Ace)],1), 15),
    ("27",f([(Clubs, Num(~10))],10),10),
    ("28",f([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42),3)
];

fun test_cases_officiate(f, mvs) = [
    ("1",f(lc1, mvs, 62), 0),
    ("2",f(#Spades number, mvs, 60), 3),
    ("3",f(#Clubs number, mvs, 50),6),
    ("4",f(#Diamonds number, mvs, 54),0),
    ("5",f(#Hearts number, mvs, 80), 13),
    ("6",f(#Spades honors, mvs, 41), 0),
    ("7",f(#Clubs honors, mvs, 42), 0),
    ("8",f(#Diamonds honors, mvs, 40), 1),
    ("9",f(#Hearts honors, mvs, 43),1),
    
    ("10",f((Clubs,Queen)::(#Spades number), mvs, 1),94),
    ("11",f((Spades,Queen)::(#Clubs number), mvs, 100), 18),
    ("12",f((Hearts,King)::(#Diamonds number), mvs, 64), 0),
    ("13",f((Diamonds,King)::(#Hearts number), mvs, 63), 1),
    
    ("14",f((Diamonds,King)::(#Spades honors), mvs, 52), 1),
    ("15",f((Hearts,King)::(#Clubs honors), mvs, 50), 3),
    ("16",f((Spades,Queen)::(#Diamonds honors), mvs, 41), 30),
    ("17",f((Clubs,Queen)::(#Hearts honors), mvs, 60), 9),
    
    ("18",f((#Spades number)@[(Diamonds,King)], mvs, 64), 0),
    ("19",f((#Clubs number)@[(Hearts,King)], mvs, 6), 174),
    ("20",f((#Diamonds number)@[(Spades,Queen)], mvs, 24), 120),
    ("21",f((#Hearts number)@[(Clubs,Queen)], mvs, 664), 600),
    ("22",f(lc2, mvs, 50), 66),
    ("23",f(shuffled, mvs, 400), 20),
    ("24",f(deck, mvs, 360), 60),
    ("25",f([], mvs,10), 5),
    ("26",f([(Clubs, Ace)], mvs, 1), 15),
    ("27",f([(Clubs, Num(~10))], mvs, 10),10),
    ("28",f([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], mvs, 42),3)
];

if run_test(testcases_all_same_colors(all_same_color))
then [("ALL TESTS PASSED FOR fun all_same_color",true,true)]
else retrieve_failed_tests(testcases_all_same_colors(all_same_color));

if run_test(testcases_sum_cards(sum_cards))
then [("ALL TESTS PASSED FOR fun sum_cards",0,0)]
else retrieve_failed_tests(testcases_sum_cards(sum_cards));

if run_test(test_cases_score(score))
then [("ALL TESTS PASSED FOR fun score",0,0)]
else retrieve_failed_tests(test_cases_score(score));

val mvs = [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw];

if run_test(test_cases_officiate(officiate,mvs))
then [("ALL TESTS PASSED FOR fun officiate",0,0)]
else retrieve_failed_tests(test_cases_officiate(officiate,mvs));

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
    end;
provided_test2();
val ms1 = [Draw,Draw,Draw,Draw,Draw,Draw];
officiate(lc1, ms1, 62);
provided_test1();
