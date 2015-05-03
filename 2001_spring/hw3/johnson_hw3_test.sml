use "johnson_hw3.sml";
val name_hw = "2001 - Homework 3 - Association Lists"

val f1=store
val f2=fetch
val f3=create_league
val f4=record_game
val f5=better_record
(*Unit tests are of format string*bool where bool is represented by evaluation of a function to an expected value*)

val nba_leagues = [
    "Atlanta Hawks",
    "Boston Celtics",
    "Charlotte Bobcats",
    "Chicago Bulls",
    "Cleveland Cavaliers",
    "Dallas Mavericks",
    "Denver Nuggets",
    "Detroit Pistons",
    "Golden State Warriors",
    "Houston Rockets",
    "Indiana Pacers",
    "LA Clippers",
    "LA Lakers",
    "Memphis Grizzlies",
    "Miami Heat",
    "Milwaukee Bucks",
    "Minnesota Timberwolves",
    "New Jersey Nets",
    "New Orleans Hornets",
    "New York Knicks",
    "Oklahoma City Thunder",
    "Orlando Magic",
    "Philadelphia Sixers",
    "Phoenix Suns",
    "Portland Trail Blazers",
    "Sacramento Kings",
    "San Antonio Spurs",
    "Toronto Raptors",
    "Utah Jazz",
    "Washington Wizards"
]
val control = [
    ("Washington Wizards",{losses=0,wins=0}),
    ("Utah Jazz",{losses=0,wins=0}),
    ("Toronto Raptors",{losses=0,wins=0}),
    ("San Antonio Spurs",{losses=0,wins=0}),
    ("Sacramento Kings",{losses=0,wins=0}),
    ("Portland Trail Blazers",{losses=0,wins=0}),
    ("Phoenix Suns",{losses=0,wins=0}),
    ("Philadelphia Sixers",{losses=0,wins=0}),
    ("Orlando Magic",{losses=0,wins=0}),
    ("Oklahoma City Thunder",{losses=0,wins=0}),
    ("New York Knicks",{losses=0,wins=0}),
    ("New Orleans Hornets",{losses=0,wins=0}),
    ("New Jersey Nets",{losses=0,wins=0}),
    ("Minnesota Timberwolves",{losses=0,wins=0}),
    ("Milwaukee Bucks",{losses=0,wins=0}),
    ("Miami Heat",{losses=0,wins=0}),
    ("Memphis Grizzlies",{losses=0,wins=0}),
    ("LA Lakers",{losses=0,wins=0}),
    ("LA Clippers",{losses=0,wins=0}),
    ("Indiana Pacers",{losses=0,wins=0}),
    ("Houston Rockets",{losses=0,wins=0}),
    ("Golden State Warriors",{losses=0,wins=0}),
    ("Detroit Pistons",{losses=0,wins=0}),
    ("Denver Nuggets",{losses=0,wins=0}),
    ("Dallas Mavericks",{losses=0,wins=0}),
    ("Cleveland Cavaliers",{losses=0,wins=0}),
    ("Chicago Bulls",{losses=0,wins=0}),
    ("Charlotte Bobcats",{losses=0,wins=0}),
    ("Boston Celtics",{losses=0,wins=0}),
    ("Atlanta Hawks",{losses=0,wins=0})
]

val tests = [
    ("1.00", f1([],"","") = [("","")]),
    ("1.01", f1([],"1","test") = [("1","test")]),
    ("1.02", f1([("0","init")],"1","test") = [("0","init"),("1","test")]),
    ("1.03", f1([("0","init"),("1","input")],"1","test") = [("0","init"),("1","test")]),
    ("1.04", f1([("0","init"),("1","input"),("2","input")],"1","test") = [("0","init"),("1","test"),("2","input")]),
    ("1.05", f1([("0","init"),("1","input"),("2","input")],"2","test") = [("0","init"),("1","input"),("2","test")]),
    ("1.06", f1([("0","init"),("1","input"),("2","input")],"0","test") = [("0","test"),("1","input"),("2","input")]),

    ("2.00", f2([],"") = "nonsense" handle NotFound => true),
    ("2.00", f2([("1","nonsense")],"") = "nonsense" handle NotFound => true),
    ("2.00", f2([("1","nonsense")],"1") = "nonsense"),
    ("2.00", f2([("1","nonsense"),("2","input")],"1") = "nonsense"),
    ("2.00", f2([("1","nonsense"),("2","input")],"2") = "input"),
    ("2.00", f2([("1","nonsense"),("2","input"),("3","init")],"0") = "nonsense" handle NotFound => true),
    ("2.00", f2([("1","nonsense"),("2","input"),("3","init")],"4") = "nonsense" handle NotFound => true),
    ("2.00", f2([("1","nonsense"),("2","input"),("3","init")],"1") = "nonsense"),
    ("2.00", f2([("1","nonsense"),("2","input"),("3","init")],"2") = "input"),
    ("2.00", f2([("1","nonsense"),("2","input"),("3","init")],"3") = "init"),

    ("3.00", f3 [] = []),
    ("3.01", f3 [""] = [("",{losses= 0, wins=0})]),
    ("3.02", f3 nba_leagues = control),

    ("4.00", f4 ([], {loser="",winner=""}) <> [] handle NotInLeague => true),
    ("4.01", f4 (f3 ["invalid"], {loser="",winner=""}) <> [] handle NotInLeague => true),
    ("4.02", f4 (f3 ["a","b","c"], {loser="",winner=""}) <> [] handle NotInLeague => true),
    ("4.03", f4 (f3 ["a","b","c"], {loser="a",winner=""}) <> [] handle NotInLeague => true),
    ("4.04", f4 (f3 ["a","b","c"], {loser="",winner="b"}) <> [] handle NotInLeague => true),
    ("4.05", f4 (f3 ["a","b","c"], {loser="c",winner=""}) <> [] handle NotInLeague => true),

    ("4.10", f4 (f3 [""], {loser="",winner=""}) = [("",{losses=1,wins=1})]),
    ("4.11", f4 (f3 ["1",""], {loser="1",winner=""}) = [("",{losses=0,wins=1}),("1",{losses=1,wins=0})]),
    ("4.12", f4 (f3 ["2","1",""], {loser="1",winner=""}) = [("",{losses=0,wins=1}),("1",{losses=1,wins=0}),("2",{losses=0,wins=0})]),

    ("5.00", f5 ({losses=0,wins=0},{losses=0,wins=0}) = false),
    ("5.01", f5 ({losses=1,wins=0},{losses=0,wins=0}) = false),
    ("5.02", f5 ({losses=0,wins=1},{losses=0,wins=0}) = true),
    ("5.03", f5 ({losses=0,wins=0},{losses=1,wins=0}) = true),
    ("5.04", f5 ({losses=0,wins=0},{losses=0,wins=1}) = false),
    ("5.05", f5 ({losses=3,wins=10},{losses=20,wins=3})=true),
    ("5.06", f5 ({losses=3,wins=10},{losses=20,wins=30})=false)
    
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
    true => print "--------------EVERY TESTS PASSED-------------\n"
  | false => print "--------------SOMETHING IS WRONG-------------------------\n";
