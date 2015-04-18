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
