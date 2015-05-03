fun alist_map (f, al) = List.map f al
fun team_names al = alist_map ((fn(k, v) => k), al)
			     
fun alist_reduce (f, b, nil) = b
  | alist_reduce (f, b, (k,v)::rest) = f(k, v, alist_reduce (f, b, rest))
fun better_record ({losses=l1,wins=w1}, {losses=l2,wins=w2}) = w1-l1 > w2-l2
fun best_record l = alist_reduce ((fn(k,v,rest)=> if better_record(v,rest) then v else rest), {losses=0,wins=0}, l)

fun all_games_back l =
  let val no1 as {losses=l1,wins=w1} = best_record l
  in alist_map ((fn(k, {losses=l2,wins=w2})=> (k,real((l2-l1)-(w2-w1))/2.0)), l)
  end
				       
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
    ("Utah Jazz",{losses=0,wins=10}),
    ("Toronto Raptors",{losses=5,wins=10}),
    ("San Antonio Spurs",{losses=30,wins=50}),
    ("Sacramento Kings",{losses=10,wins=70}),
    ("Portland Trail Blazers",{losses=10,wins=90}),
    ("Phoenix Suns",{losses=100,wins=60}),
    ("Philadelphia Sixers",{losses=90,wins=20}),
    ("Orlando Magic",{losses=70,wins=10}),
    ("Oklahoma City Thunder",{losses=60,wins=2}),
    ("New York Knicks",{losses=0,wins=4}),
    ("New Orleans Hornets",{losses=40,wins=6}),
    ("New Jersey Nets",{losses=30,wins=7}),
    ("Minnesota Timberwolves",{losses=0,wins=100}),
    ("Milwaukee Bucks",{losses=50,wins=110}),
    ("Miami Heat",{losses=10,wins=120}),
    ("Memphis Grizzlies",{losses=0,wins=130}),
    ("LA Lakers",{losses=120,wins=150}),
    ("LA Clippers",{losses=110,wins=90}),
    ("Indiana Pacers",{losses=100,wins=80}),
    ("Houston Rockets",{losses=90,wins=70}),
    ("Golden State Warriors",{losses=80,wins=60}),
    ("Detroit Pistons",{losses=70,wins=50}),
    ("Denver Nuggets",{losses=60,wins=40}),
    ("Dallas Mavericks",{losses=50,wins=30}),
    ("Cleveland Cavaliers",{losses=40,wins=20}),
    ("Chicago Bulls",{losses=30,wins=10}),
    ("Charlotte Bobcats",{losses=20,wins=30}),
    ("Boston Celtics",{losses=20,wins=90}),
    ("Atlanta Hawks",{losses=10,wins=45})
]
		  
all_games_back control
