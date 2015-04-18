val d1 = (1981,7,5)
val d2 = (1982,1,1)
val d3 = (1981,6,15)
val d4 = (1981,8,1)
val d5 = (1981,7,1)
val d6 = (1981,7,15)
val d7 = (1981,~7,5)
val d8 = (~1982,7,5);

is_older(d1,d2) = true;
is_older(d1,d3) = false;
is_older(d1,d4) = true;				   
is_older(d1,d5) = false;
is_older(d1,d6) = true;
is_older(d1,d7) = false;
is_older(d1,d8) = false;
is_older(d1,d1) = false;

val ld = [d1, d2, d3, d4, d5, d6, d7, d8];
number_in_month(ld, 1) = 1;
number_in_month(ld, 2) = 0;
number_in_month(ld, 7) = 4;
number_in_month(ld, 6) = 1;
number_in_month(ld, 8) = 1;
number_in_month(ld, ~7) = 1;
