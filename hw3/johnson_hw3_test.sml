use "johnson_hw3.sml";

val tests = [
    ("1.0 Empty List", only_capitals([]) = []),
    ("1.10 One element List: Empty String", only_capitals([""]) = []),
    ("1.11 One element List: Lower Case letter", only_capitals(["t"]) = []),
    ("1.12 One element List: Lower Case Word", only_capitals(["test"]) = []),
    ("1.13 One element List: Lower Case Word with Capital letter", only_capitals(["tEst"]) = []),
    ("1.14 One element List: Capitalized Word", only_capitals(["Test"]) = ["Test"]),
    ("1.20 Two element List: Empty String", only_capitals(["",""]) = []),
    ("1.21 Two element List: Empty String + lower case", only_capitals(["","t"]) = []),
    ("1.22 Two element List: Empty String + lower case word", only_capitals(["","test"]) = []),
    ("1.23 Two element List: Empty String + lower case word with capital letter", only_capitals(["","teSt"]) = []),
    ("1.24 Two element List: Empty String + Capitalized at end", only_capitals(["","Test"]) = ["Test"]),
    ("1.25 Two element List: Empty String + Capitalized at front", only_capitals(["Test", ""]) = ["Test"]),
    ("1.30 Three element List: 2*Empty String + Capitalized at end", only_capitals(["","","Test"]) = ["Test"]),
    ("1.31 Three element List: 2*Empty String + Capitalized at fron", only_capitals(["Test","",""]) = ["Test"]),
    ("1.32 Three element List: 2*Empty String + Capitalized in middle", only_capitals(["","Test",""]) = ["Test"]),
    ("1.33 Three element List: 2*lower case + Capitalized at end", only_capitals(["est","st","Test"]) = ["Test"]),
    ("1.34 Three element List: 2*lower case + Capitalized at fron", only_capitals(["Test","est","st"]) = ["Test"]),
    ("1.35 Three element List: 2*lower case + Capitalized in middle", only_capitals(["st","Test","est"]) = ["Test"]),
    ("1.40 Plenty element List: NONE capitalized", only_capitals(["s6CmxYH9KAr","su40waWdFvM","s6CmxYH9KAr","sDW0wQKwWu7","sASt81VnTZY","sENkyvsBJf6","sfPvrv723iB","su40waWdFvM","sb9lgY0DxVo","sb9lgY0DxVo"]) = []),
    ("1.41 Plenty element List: ALL capitalized", only_capitals(["Cu40waWdFvM","CfPvrv723iB","CfPvrv723iB","CfPvrv723iB","Cb9lgY0DxVo","CDKoLm70Avn","CDW0wQKwWu7","Cb9lgY0DxVo","CDW0wQKwWu7","C1bXG9fplwP"]) = ["Cu40waWdFvM","CfPvrv723iB","CfPvrv723iB","CfPvrv723iB","Cb9lgY0DxVo","CDKoLm70Avn","CDW0wQKwWu7","Cb9lgY0DxVo","CDW0wQKwWu7","C1bXG9fplwP"]),
    ("1.42 Plenty element List: SOME capitalized SOME not", only_capitals(["ENkyvsBJf6","1bXG9fplwP","u40waWdFvM","fPvrv723iB","DKoLm70Avn","b9lgY0DxVo","fPvrv723iB","ENkyvsBJf6","u40waWdFvM","u40waWdFvM"]) = ["ENkyvsBJf6","DKoLm70Avn","ENkyvsBJf6"]),

    ("2.0 Empty List", longest_string1([]) = ""),
    ("2.10 One element List: Empty String", longest_string1([""]) = ""),
    ("2.11 One element List: one word", longest_string1(["test"]) = "test"),
    
    ("2.20 Two element List: Empty String", longest_string1(["",""]) = ""),
    ("2.21 Two element List: Empty String + one word", longest_string1(["","test"]) = "test"),
    ("2.22 Two element List: One word + Empty String", longest_string1(["test",""]) = "test"),
    ("2.23 Two element List: 2 Words: Longest first", longest_string1(["testTest","teSt"]) = "testTest"),
    ("2.24 Two element List: 2 Words: Shortest first", longest_string1(["test","testTest"]) = "testTest"),
    ("2.25 Two element List: 2 Words: Equal Length", longest_string1(["testTest1","testTest2"]) = "testTest1"),
    
    ("2.30 Three element List: 2*Empty String + Word at end", longest_string1(["","","Test"]) = "Test"),
    ("2.31 Three element List: 2*Empty String + Word at fron", longest_string1(["Test","",""]) = "Test"),
    ("2.32 Three element List: 2*Empty String + Word in middle", longest_string1(["","Test",""]) = "Test"),
    ("2.33 Three element List: 2*Short Word + Word at end", longest_string1(["est","sts","Test"]) = "Test"),
    ("2.34 Three element List: 2*Short Word + Word at fron", longest_string1(["Test","est","ste"]) = "Test"),
    ("2.35 Three element List: 2*Short Word + Word in middle", longest_string1(["ste","Test","est"]) = "Test"),
    ("2.36 Three element List: 2*Equal Long Word + Word in middle", longest_string1(["testTest1","Test","testTest2"]) = "testTest1"),
    
    ("2.40 Plenty element List: ALL equal length", longest_string1(["s6CmxYH9KAr","su40waWdFvM","s6CmxYH9KAr","sDW0wQKwWu7","sASt81VnTZY","sENkyvsBJf6","sfPvrv723iB","su40waWdFvM","sb9lgY0DxVo","sb9lgY0DxVo"]) = "s6CmxYH9KAr"),
    ("2.41 Plenty element List: ALL Different Length. Big To Small", longest_string1(["Cu40waWdFvM","CfPvrv723i","CfPvrv723","CfPvrv72","Cb9lgY0","CDKoLm","CDW0w","Cb9l","CDW","C1"]) = "Cu40waWdFvM"),
    ("2.42 Plenty element List: All Different Length. Small To Big", longest_string1(["E","1bX","u40","fPvr","DKoLm","b9lgY0","fPvrv72","ENkyvsBJ","u40waWdFv","u40waWdFvM"]) = "u40waWdFvM"),
    ("2.43 Plenty element List: All Different Length. Random", longest_string1(["E","DKoLm","b9lgY0","1bX","fPvr","u40waWdFvM","fPvrv72","ENkyvsBJ","u40","u40waWdFv"]) = "u40waWdFvM"),

    ("3.0 Empty List", longest_string2([]) = ""),
    ("3.10 One element List: Empty String", longest_string2([""]) = ""),
    ("3.11 One element List: one word", longest_string2(["test"]) = "test"),
    
    ("3.20 Two element List: Empty String", longest_string2(["",""]) = ""),
    ("3.21 Two element List: Empty String + one word", longest_string2(["","test"]) = "test"),
    ("3.22 Two element List: One word + Empty String", longest_string2(["test",""]) = "test"),
    ("3.23 Two element List: 2 Words: Longest first", longest_string2(["testTest","teSt"]) = "testTest"),
    ("3.24 Two element List: 2 Words: Shortest first", longest_string2(["test","testTest"]) = "testTest"),
    ("3.25 Two element List: 2 Words: Equal Length", longest_string2(["testTest1","testTest2"]) = "testTest2"),
    
    ("3.30 Three element List: 2*Empty String + Word at end", longest_string2(["","","Test"]) = "Test"),
    ("3.31 Three element List: 2*Empty String + Word at fron", longest_string2(["Test","",""]) = "Test"),
    ("3.32 Three element List: 2*Empty String + Word in middle", longest_string2(["","Test",""]) = "Test"),
    ("3.33 Three element List: 2*Short Word + Word at end", longest_string2(["est","sts","Test"]) = "Test"),
    ("3.34 Three element List: 2*Short Word + Word at fron", longest_string2(["Test","est","ste"]) = "Test"),
    ("3.35 Three element List: 2*Short Word + Word in middle", longest_string2(["ste","Test","est"]) = "Test"),
    ("3.36 Three element List: 2*Equal Long Word + Word in middle", longest_string2(["testTest1","Test","testTest2"]) = "testTest2"),
    
    ("3.40 Plenty element List: ALL equal length", longest_string2(["s6CmxYH9KAr","su40waWdFvM","s6CmxYH9KAr","sDW0wQKwWu7","sASt81VnTZY","sENkyvsBJf6","sfPvrv723iB","su40waWdFvM","sb9lgY0DxVo","sb9lgY0DxVo"]) = "sb9lgY0DxVo"),
    ("3.41 Plenty element List: ALL Different Length. Big To Small", longest_string2(["Cu40waWdFvM","CfPvrv723i","CfPvrv723","CfPvrv72","Cb9lgY0","CDKoLm","CDW0w","Cb9l","CDW","C1"]) = "Cu40waWdFvM"),
    ("3.42 Plenty element List: All Different Length. Small To Big", longest_string2(["E","1bX","u40","fPvr","DKoLm","b9lgY0","fPvrv72","ENkyvsBJ","u40waWdFv","u40waWdFvM"]) = "u40waWdFvM"),
    ("3.43 Plenty element List: All Different Length. Random", longest_string2(["E","DKoLm","b9lgY0","1bX","fPvr","u40waWdFvM","fPvrv72","ENkyvsBJ","u40","u40waWdFv"]) = "u40waWdFvM"),



    ("5.0 Empty List", longest_capitalized [] = ""),
    ("5.10 One element List: Empty String", longest_capitalized [""] = ""),
    ("5.11 One element List: one word", longest_capitalized ["test"] = ""),
    ("5.12 One element List: one word", longest_capitalized ["Test"] = "Test"),
    
    ("5.20 Two element List: Empty String", longest_capitalized ["",""] = ""),
    ("5.21 Two element List: Empty String + one word", longest_capitalized ["","test"] = ""),
    ("5.22 Two element List: One word + Empty String", longest_capitalized ["test",""] = ""),
    ("5.23 Two element List: 2 Words: Longest first", longest_capitalized ["testTest","teSt"] = ""),
    ("5.24 Two element List: 2 Words: Shortest first", longest_capitalized ["test","testTest"] = ""),
    ("5.25 Two element List: 2 Words: Equal Length", longest_capitalized ["testTest1","testTest2"] = ""),
    ("5.26 Two element List: Empty String + one word", longest_capitalized ["","Test"] = "Test"),
    ("5.27 Two element List: One word + Empty String", longest_capitalized ["Test",""] = "Test"),
    ("5.28 Two element List: 2 Words: Longest first", longest_capitalized ["testTest","TeSt"] = "TeSt"),
    ("5.29a Two element List: 2 Words: Shortest first", longest_capitalized ["Test","testTest"] = "Test"),
    ("5.29b Two element List: 2 Words: Equal Length", longest_capitalized ["TestTest1","TestTest2"] = "TestTest1"),
    
    ("5.30 Three element List: 2*Empty String + Word at end", longest_capitalized ["","","Test"] = "Test"),
    ("5.31 Three element List: 2*Empty String + Word at fron", longest_capitalized ["Test","",""] = "Test"),
    ("5.32 Three element List: 2*Empty String + Word in middle", longest_capitalized ["","Test",""] = "Test"),
    ("5.33 Three element List: 2*Short Word + Word at end", longest_capitalized ["est","sts","Test"] = "Test"),
    ("5.34 Three element List: 2*Short Word + Word at fron", longest_capitalized ["Test","est","ste"] = "Test"),
    ("5.35 Three element List: 2*Short Word + Word in middle", longest_capitalized ["ste","Test","est"] = "Test"),
    ("5.36 Three element List: 2*Equal Long Word + Word in middle", longest_capitalized ["TestTest1","Test","testTest2"] = "TestTest1"),
    ("5.37 Three element List: 2*Equal Long Word + Word in middle", longest_capitalized ["testTest1","Test","TestTest2"] = "TestTest2"),
    ("5.38 Three element List: 2*Equal Long Word + Word in middle", longest_capitalized ["TestTest1","Test","TestTest2"] = "TestTest1"),
    
    ("5.40 Plenty element List: ALL equal length", longest_capitalized ["Ss6CmxYH9KAr","Ssu40waWdFvM","Ss6CmxYH9KAr","SsDW0wQKwWu7","SsASt81VnTZY","SsENkyvsBJf6","SsfPvrv723iB","Ssu40waWdFvM","Ssb9lgY0DxVo","Ssb9lgY0DxVo"] = "Ss6CmxYH9KAr"),
    ("5.41 Plenty element List: ALL Different Length. Big To Small", longest_capitalized ["Cu40waWdFvM","CfPvrv723i","CfPvrv723","CfPvrv72","Cb9lgY0","CDKoLm","CDW0w","Cb9l","CDW","C1"] = "Cu40waWdFvM"),
    ("5.42 Plenty element List: All Different Length. Small To Big", longest_capitalized ["SE","S1bX","Su40","SfPvr","SDKoLm","Sb9lgY0","SfPvrv72","SENkyvsBJ","Su40waWdFv","Su40waWdFvM"] = "Su40waWdFvM"),
    ("5.43 Plenty element List: All Different Length. Random", longest_capitalized ["SE","SDKoLm","Sb9lgY0","S1bX","SfPvr","Su40waWdFvM","SfPvrv72","SENkyvsBJ","Su40","Su40waWdFv"] = "Su40waWdFvM"),

    ("6.10 Empty String", rev_string("") = ""),
    ("6.11 one letter", rev_string("t") = "t"),
    ("6.12 one word", rev_string("Test") = "tseT"),

    ("9b.0 Wildcard", count_wildcards(Wildcard) = 1),
    ("9b.1 Variable x", count_wildcards(Variable("test")) = 0),
    ("9b.2 UnitP", count_wildcards(UnitP) = 0),
    ("9b.3 ConstP(3)", count_wildcards(ConstP(3)) = 0),
    ("9b.40 TupleP([])", count_wildcards(TupleP[]) = 0),
    ("9b.41 TupleP([Wildcard])", count_wildcards(TupleP([Wildcard])) = 1),
    ("9b.42 TupleP([Variable x])", count_wildcards(TupleP([Variable("Wildcard")])) = 0),
    ("9b.43 TupleP([UnitP])", count_wildcards(TupleP([UnitP])) = 0),
    ("9b.44 TupleP([ConstP(5)])", count_wildcards(TupleP([ConstP(5)])) = 0),
    ("9b.45 TupleP([Wildcard,..])", count_wildcards(TupleP([Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)])) = 5),
   
    ("9b.500 ConstructorP(,Wildcard)", count_wildcards(ConstructorP("test",Wildcard)) = 1),
    ("9b.501 ConstructorP(,Variable)", count_wildcards(ConstructorP("test",Variable("Wildcard"))) = 0),
    ("9b.502 ConstructorP(,UnitP)", count_wildcards(ConstructorP("test",UnitP)) = 0),
    ("9b.503 ConstructorP(,ConstP)", count_wildcards(ConstructorP("test",ConstP(4))) = 0),
    ("9b.504 ConstructorP(,TupleP[])", count_wildcards(ConstructorP("test",TupleP[])) = 0),
    ("9b.505 ConstructorP(,TupleP[Wildcard)", count_wildcards(ConstructorP("test",TupleP[Wildcard])) = 1),
    ("9b.506 ConstructorP(,TupleP[Variable)", count_wildcards(ConstructorP("test",TupleP[Variable("Wildcard")])) = 0),
    ("9b.507 ConstructorP(,TupleP[UnitP)", count_wildcards(ConstructorP("test",TupleP[UnitP])) = 0),
    ("9b.508 ConstructorP(,TupleP[ConstP)", count_wildcards(ConstructorP("test",TupleP[ConstP(5)])) = 0),
    ("9b.509 ConstructorP(,TupleP[....)", count_wildcards(ConstructorP("test",TupleP[Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)])) = 5),
    ("9b.510 ConstructorP(,Constructor(,Wildcard)", count_wildcards(ConstructorP("test",ConstructorP("test2",Wildcard))) = 1),
    ("9b.511 ConstructorP(,Constructor(,Variable)", count_wildcards(ConstructorP("test",ConstructorP("test2",Variable("Wildcard")))) = 0),
    ("9b.512 ConstructorP(,Constructor(,UnitP)", count_wildcards(ConstructorP("test",ConstructorP("test2",UnitP))) = 0),
    ("9b.513 ConstructorP(,Constructor(,ConstP)", count_wildcards(ConstructorP("test",ConstructorP("test2",ConstP(4)))) = 0),
    ("9b.514 ConstructorP(,Constructor(,TupleP)", count_wildcards(ConstructorP("test",ConstructorP("test2",TupleP[]))) = 0),
    ("9b.515 ConstructorP(,Constructor(,TupleP[Wildcard)", count_wildcards(ConstructorP("test",ConstructorP("test2",TupleP[Wildcard]))) = 1),
    ("9b.516 ConstructorP(,Constructor(,TupleP[Variable)", count_wildcards(ConstructorP("test",ConstructorP("test2",TupleP[Variable("Wildcard")]))) = 0),
    ("9b.517 ConstructorP(,Constructor(,TupleP[UnitP)", count_wildcards(ConstructorP("test",ConstructorP("test2",TupleP[UnitP]))) = 0),
    ("9b.518 ConstructorP(,Constructor(,TupleP[ConstP)", count_wildcards(ConstructorP("test",ConstructorP("test2",TupleP[ConstP(5)]))) = 0),
    ("9b.519 ConstructorP(,Constructor(,TupleP[......)", count_wildcards(ConstructorP("test",ConstructorP("test2",TupleP[Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)]))) = 5),

    ("9b.520 (TupleP[ConstructorP(,WILDCARD)", count_wildcards(TupleP[ConstructorP("test",ConstructorP("test2",Wildcard)),Wildcard]) = 2),
    ("9b.521 (TupleP[ConstructorP(,Variable)", count_wildcards(TupleP[ConstructorP("test",ConstructorP("test2",Variable("Wildcard"))),Wildcard]) = 1),
    ("9b.522 (TupleP[ConstructorP(,UnitP)", count_wildcards(TupleP[ConstructorP("test",ConstructorP("test2",UnitP)),UnitP,Wildcard]) = 1),
    ("9b.523 (TupleP[ConstructorP(,ConstP)", count_wildcards(TupleP[ConstructorP("test",ConstructorP("test2",ConstP(4))),ConstP(3),Wildcard]) = 1),
    ("9b.524 (TupleP[ConstructorP(,TupleP)", count_wildcards(TupleP[ConstructorP("test",ConstructorP("test2",TupleP[])),TupleP[],Wildcard]) = 1),
    ("9b.525 (TupleP[ConstructorP(,Constructor)", count_wildcards(TupleP[ConstructorP("test",ConstructorP("test2",TupleP[Wildcard])),ConstructorP("test3",Wildcard)]) = 2),

    ("9c.0 ", count_wild_and_variable_lengths(Wildcard) = 1),
    ("9c.1 ", count_wild_and_variable_lengths(Variable("Test")) = 4),
    ("9c.2 ", count_wild_and_variable_lengths(UnitP) = 0),
    ("9c.3 ", count_wild_and_variable_lengths(ConstP(2)) = 0),
    ("9c.40 ", count_wild_and_variable_lengths(TupleP([])) = 0),
    ("9c.41 ", count_wild_and_variable_lengths(TupleP([Variable("Test"),Wildcard])) = 5),
    ("9c.42 ", count_wild_and_variable_lengths(TupleP([Wildcard,UnitP])) = 1),
    ("9c.43 ", count_wild_and_variable_lengths(TupleP([Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)])) = 13),
    ("9c.44 ", count_wild_and_variable_lengths(ConstructorP("test",ConstructorP("test2",TupleP[Wildcard,Wildcard,UnitP,Wildcard,Variable("Wildcard"),Wildcard,Wildcard,ConstP(2)]))) = 13),
    ("9c.45 ", count_wild_and_variable_lengths(TupleP([ConstructorP("test",ConstructorP("test2",TupleP[Wildcard])),ConstructorP("test3",Wildcard)])) = 2),
    ("9c.46 ", count_wild_and_variable_lengths(TupleP([Variable("Test1"),Variable("Test2")])) = 10),
    ("9c.47 ", count_wild_and_variable_lengths(TupleP([Variable("Test1"),Wildcard,Variable("Test2")])) = 11),
    ("9c.48 ", count_wild_and_variable_lengths(TupleP([Variable("Test1"),ConstructorP("Wildcard",Wildcard),Variable("Test2")])) = 11),

    ("9d.0 ", count_some_var ("test") (Variable("test")) = 1),
    ("9d.0 ", count_some_var ("dates") (Wildcard) = 0),
    ("9d.0 ", count_some_var ("dates") (ConstP(3)) = 0),
    ("9d.0 ", count_some_var ("setad") (UnitP) = 0),
    ("9d.0 ", count_some_var ("wert") (TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 0),
    ("9d.0 ", count_some_var ("") (TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 1),
    ("9d.0 ", count_some_var ("test") (TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 1),
    ("9d.0 ", count_some_var ("Wert") (TupleP[Wildcard,Variable("test"),Variable(""),Variable("dates"),ConstP(2),Variable("Wert")]) = 1),
    ("9d.0 ", count_some_var ("wert") (TupleP[Wildcard,Variable("test"),Variable("wert"),Variable("dates"),ConstP(2),Variable("wert"),Wildcard,ConstructorP("wert",Wildcard),ConstructorP("wert",Variable("wert")),ConstructorP("test",Variable("wert"))]) = 4)
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
  | false => print "--------------SOMETHING IS WRONG-------------------------\n"

