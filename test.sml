val x = 3;
fun pow(x, y) =
  if y=0
  then 1
  else x * pow(x, y - 1);

fun cube (x) =
  pow(x, 3);

fun swap(pr: int*bool) =
  (#2 pr, #1 pr);

fun sum_two_pairs(pr1: int*int, pr2: int*int) =
  (#1 pr1 + #1 pr2, #2 pr1 + #2 pr2);

fun summation(pr1: int*int, pr2: int*int) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2);

fun div_mod(x: int, y: int) =
  (x div y, x mod y);

fun sort_pair(pr: int*int) =
  if (#1 pr) < (#2 pr)
  then pr
  else ((#2 pr), (#1 pr));

fun sum_list (xs: int list) =
  if null xs
  then 0
  else hd(xs) + sum_list(tl xs);

fun count_down (x: int) =
  if x = 0
  then []
  else x::count_down(x - 1);

fun append(xs, ys: int list) =
  if null xs
  then ys
  else (hd xs)::append(tl xs, ys);

fun sum_pair_list(xs: (int*int) list) =
  if null xs
  then 0
  else (#1 (hd xs)) + (#2 (hd xs)) + sum_pair_list(tl xs);

fun firsts (xs: (int*int) list) =
  if null xs
  then []
  else (#1 (hd xs))::firsts(tl xs);

fun seconds (xs: (int*int) list) =
  if null xs
  then []
  else (#2 (hd xs))::seconds(tl xs);

val result = seconds([(1, 2), (3, 4), (5, 6)]);

fun count_up (x: int) =
  let fun count(from: int) =
    if from > x
    then []
    else from::count(from + 1)
  in
    count 1
  end
val test_count = count_up(10);

fun good_max(xs: int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else let val temp = good_max(tl xs)
  in
    if hd xs > temp
    then hd xs
    else temp
  end

val test_max = good_max([1,2,3,4,5,100,5,33,7]);
