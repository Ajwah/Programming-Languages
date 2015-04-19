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
	   
