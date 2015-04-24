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
    ("3.43 Plenty element List: All Different Length. Random", longest_string2(["E","DKoLm","b9lgY0","1bX","fPvr","u40waWdFvM","fPvrv72","ENkyvsBJ","u40","u40waWdFv"]) = "u40waWdFvM")
	
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

