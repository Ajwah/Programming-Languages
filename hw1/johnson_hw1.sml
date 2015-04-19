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

fun what_month(d: int) =
  let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in number_before_reaching_sum(d, months) + 1
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else let fun range(from: int, to: int) =
	     if from = to
	     then [to]
	     else from::range(from + 1, to)
       in range(what_month(day1), what_month(day2))
       end
	   
fun oldest(l: (int*int*int) list) =
  if null l
  then NONE
  else let val max = oldest(tl l)
       in if max = NONE
	  then SOME (hd l)
	  else if is_older(hd l, valOf max)
	  then SOME (hd l)
	  else max
       end

fun oldest_test(l: int list) =
  if null l
  then NONE
  else let val max = oldest_test(tl l)
       in if max = NONE
	  then SOME (hd l)
	  else if hd l < valOf max
	  then SOME (hd l)
	  else max
       end;

fun remove_duplicates(l: int list) =
  let fun find(el: int, l: int list) =
	if null l
	then false
	else if el = hd l
	then true
	else find(el, tl l)
  in 
      if null l
      then []
      else if find(hd l, tl l)
      then remove_duplicates(tl l)
      else (hd l)::remove_duplicates(tl l)
  end

fun number_in_months_challenge(ld: (int*int*int) list, lm: int list) =
  number_in_months(ld, remove_duplicates(lm))

fun dates_in_months_challenge(ld: (int*int*int) list, lm: int list) =
  dates_in_months(ld, remove_duplicates(lm))

fun reasonable_date(d: int*int*int) =
  let
      val year = #1 d
      val month = #2 d 
      val day = #3 d
      val is_leapyear = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
      val is_reasonable_year = year > 0
      val is_reasonable_month = month > 0 andalso month < 13
      val get_months =
	if is_leapyear
	then [31,29,31,30,31,30,31,31,30,31,30,31]
	else [31,28,31,30,31,30,31,31,30,31,30,31]
      fun get_max_days_of_month(ml: int list, m: int) =
	if m = 1 orelse not is_reasonable_month (*important bug fix. if invalid months are inputted then uncaught exception even though get_max_days_of_month should not get evaluated as it should get short circuited at failure of is_reasonable_month conjunction*)
	then hd ml
	else get_max_days_of_month(tl ml, m - 1)
      val is_reasonable_day = day > 0 andalso day <= get_max_days_of_month(get_months, month)
  in is_reasonable_year andalso is_reasonable_month andalso if is_reasonable_month
							    then is_reasonable_day
							    else false
  end;

use "johnson_hw1_test.sml";
