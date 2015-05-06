type grid_ship = (int*int) list
datatype orientation = Horiz | Vert
type ship = { loc : (int*int), dir : orientation, size : int }
type shot_summary = {hits:int, misses:int}
			
fun make_horiz 0 = []
  | make_horiz n = (n,0)::(make_horiz (n-1))

fun make_vert 0 = []
  | make_vert n = (0,n)::(make_vert (n-1))

fun shift ship (c1,c2) = List.map (fn(x,y) => (x+c1,y+c2)) ship

fun ship_to_grid {loc=loc,dir=dir,size=s} = case dir of
						Horiz => shift (make_horiz s) loc
					      | Vert => shift (make_vert s) loc
fun fleet_to_grid [] = []
  | fleet_to_grid (x::xs') = (fleet_to_grid xs')@(ship_to_grid x)

fun shot_hit (sx,sy) = List.exists (fn(x,y)=> x = sx andalso y = sy)
						     
fun fleet_salvo_summary salvo fleet =
  let fun {hits=h1,misses=m1} + {hits=h2,misses=m2} = {hits=(h1-(~h2)),misses=(m1-(~m2))}
      fun salvo_hits [] _ = {hits=0,misses=0}
	| salvo_hits _ [] = {hits=0,misses=0}
	| salvo_hits (one_shot::[]) (one_ship::[]) = if (shot_hit one_shot one_ship)
						     then {hits=1,misses=0}
						     else {hits=0,misses=1}
	| salvo_hits (shot::salvo) (ls as (one_ship::[])) = (salvo_hits (shot::[]) ls) + (salvo_hits salvo ls)
	| salvo_hits (sl as (one_shot::[])) (ship::fleet) = (salvo_hits sl (ship::[])) + (salvo_hits sl fleet) 
	| salvo_hits (shot::salvo) (fleet as (ship::fleet')) = (salvo_hits (shot::[]) fleet) + (salvo_hits salvo fleet)
  in salvo_hits salvo fleet
  end

      (*SECOND PART - BINARY*)

datatype bit = One | Zero
type binary_number = bit list
			 
fun eval_bin bl =
  let fun eval [] _ = 0
	| eval (bit::bits) c =
	  case (bit,c > 0) of
	      (Zero,false) => 0
	    | (One,false) => 1
	    | (Zero,true) => eval bits (c-1)
	    | (One,true) => ceil(Math.pow(2.0,real(c))) + eval bits (c-1)
  in eval bl (List.length(bl)-1)
  end;
      eval_bin [One,Zero,Zero] = 4;
