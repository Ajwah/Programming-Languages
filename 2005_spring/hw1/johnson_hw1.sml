
fun ship_size ship = List.length ship
fun shot_hit (sx,sy) = List.exists (fn(x,y)=> x = sx andalso y = sy)
fun ships_collided [] _ = false
  | ships_collided (xy::[]) sh2  = shot_hit xy sh2
  | ships_collided (xy::xys') sh2 = (shot_hit xy sh2) orelse (ships_collided xys' sh2)
fun biggest_ship [] = 0
  | biggest_ship (ship::[]) = ship_size ship
  | biggest_ship (ship1::ship2::[]) = if (ship_size ship1) > (ship_size ship2) then (ship_size ship1) else (ship_size ship2)
  | biggest_ship (s1::s2::ls') =
    let val greatest = biggest_ship (s2::ls')
	val size_s1 = ship_size s1
    in if size_s1 > greatest then size_s1 else greatest
    end
fun salvo_hits [] _ = 0
  | salvo_hits _ [] = 0
  | salvo_hits (one_shot::[]) (one_ship::[]) = if (shot_hit one_shot one_ship) then 1 else 0
  | salvo_hits (shot::salvo) (ls as (one_ship::[])) = (salvo_hits (shot::[]) ls) + (salvo_hits salvo ls)
  | salvo_hits (sl as (one_shot::[])) (ship::fleet) = (salvo_hits sl (ship::[])) + (salvo_hits sl fleet) 
  | salvo_hits (shot::salvo) (fleet as (ship::fleet')) = (salvo_hits (shot::[]) fleet) + (salvo_hits salvo fleet)
											     
fun valid_damage_reports [] = true
  | valid_damage_reports ((size,hits)::[]) = hits <= size
  | valid_damage_reports (report::rest) = (valid_damage_reports (report::[])) andalso
					  (valid_damage_reports rest)

fun ship_sunk1 [] = []
  | ship_sunk1 ((size,hits)::[]) = if hits = size then [(size,hits)] else []
  | ship_sunk1 (report::rest) = (ship_sunk1 rest)@(ship_sunk1 (report::[]))

fun ship_sunk [] = NONE
  | ship_sunk ((size,hits)::[]) = if hits = size then SOME [(size,hits)] else NONE
  | ship_sunk (report::rest) = case ((ship_sunk rest),(ship_sunk (report::[]))) of
				   (NONE,NONE) => NONE
				 | (NONE,SOME r) => SOME r
				 | (SOME r, NONE) => SOME r
				 | (SOME r, SOME r') => SOME (r@r')
							     
