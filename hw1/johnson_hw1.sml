fun is_older(d1: int*int*int, d2: int*int*int) =
  if (#1 d1) < (#1 d2)
  then true
  else if (#1 d1) = (#1 d2) andalso (#2 d1) < (#2 d2)
  then true
  else if (#2 d1) = (#2 d2) andalso (#3 d1) < (#3 d2)
  then true
  else false

fun number_in_month(ld: (int*int*int) list, m: int) =
  if null ld
  then 0
  else if #2 (hd ld) = m
  then 1 + number_in_month(tl ld, m)
  else number_in_month(tl ld, m)

fun number_in_months(ld: (int*int*int) list, lm: int list) =
  if null lm
  then 0
  else number_in_month(ld, hd lm) + number_in_months(ld, tl lm)

fun dates_in_month(ld: (int*int*int) list, m: int) =
  if null ld
  then []
  else let val md = (hd ld)
       in if #2 md = m
	  then md::dates_in_month(tl ld, m)
	  else dates_in_month(tl ld, m)
       end

fun dates_in_months(ld: (int*int*int) list, lm: int list) =
  if null lm
  then []
  else dates_in_month(ld, hd lm) @ dates_in_months(ld, tl lm)
						  
fun get_nth(ls: string list, n: int) =
  if n = 1
  then hd ls
  else get_nth(tl ls, n - 1)

fun date_to_string(d: int*int*int) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
      val year = Int.toString(#1 d)
      val month = #2 d
      val day = " " ^ Int.toString(#3 d) ^ ", "
  in get_nth(months, month) ^ day ^ year
  end

fun number_before_reaching_sum(sum: int, l: int list) =
  let fun summation(s: int, c: int, l: int list) =
	if s >= sum
	then c
	else summation(s + hd l, c + 1, tl l)
  in summation(0, ~1, l)
  end

     
