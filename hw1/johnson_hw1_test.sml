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
number_in_month([], 7) = 0;

val lm1 = [0, 1, 2, 3, ~4];
val lm2 = [1, 7, ~7, 8, 6];
val lm3 = [1000, 1981, 1982, ~1982];
val lm4 = [5, ~1, 15];
val lm5 = [1, ~7, 8, 6];
val lm6 = [7];
number_in_months(ld, lm1) = 1;
number_in_months(ld, lm2) = 8;
number_in_months(ld, lm3) = 0;
number_in_months(ld, lm4) = 0;
number_in_months(ld, lm5) = 4;
number_in_months(ld, lm6) = 4;
number_in_months([], lm2) = 0;
number_in_months(ld, []) = 0;

dates_in_month(ld, 1) = [d2];
dates_in_month(ld, 2) = [];
dates_in_month(ld, 7) = [d1, d5, d6, d8];
dates_in_month(ld, 6) = [d3];
dates_in_month(ld, 8) = [d4];
dates_in_month(ld, ~7) = [d7];
dates_in_month([], 7) = [];

dates_in_months(ld, lm1) = [d2];
dates_in_months(ld, lm2) = [d2,d1,d5,d6,d8,d7,d4,d3];
dates_in_months(ld, lm3) = [];
dates_in_months(ld, lm4) = [];
dates_in_months(ld, lm5) = [d2,d7,d4,d3];
dates_in_months(ld, lm6) = [d1,d5,d6,d8];
dates_in_months([], lm2) = [];
dates_in_months(ld, []) = [];

val list_strings = [
    "DhM7n0CXyfc764",
    "33fSUM0T6GMTbl",
    "Vori9qgRabWRBj",
    "M0Uu9wGyrrae4U",
    "kKrWt6IJcuHOy4",
    "R5mFpbLvRNyIFn",
    "NEgZTUqj9Zt11j",
    "I4JHoca8bp52r1",
    "Zbm2T7eVRy7coq",
    "mMKzXyaQQcZEcj",
    "qbf0z543za85me",
    "VzA9QvTG32IqHj",
    "9Uq8NXLWtFjoUH",
    "MLnwpc5GBxeXRN",
    "5oFjFZe0cvMqFv",
    "7hxHW4MS6vQHAa",
    "AuB7uZ0rokrD8g",
    "Wx8sqNiyl3BfAD",
    "4DLNSrqy9HMvz1",
    "o1AGwZO7I1HB7t",
    "bTlVgBhV4Kl43e",
    "3HnGsTpQ0KNYa3",
    "LHIq5yYCGGAGst",
    "S2eDPcrjtsrSVP",
    "pjpUqzKyqgykG6",
    "qvotiDks0PRzet",
    "4c21Mlak1lXfCj",
    "0krc3fYtFH0foE",
    "55mL3uLrHCs7OK",
    "T2WJ7CIEwuXbBz"
];

get_nth(list_strings, 1) = "DhM7n0CXyfc764";
get_nth(list_strings, 10) = "mMKzXyaQQcZEcj";
get_nth(list_strings, 15) = "5oFjFZe0cvMqFv";
get_nth(list_strings, 3) = "Vori9qgRabWRBj";

date_to_string(d1) = "July 5, 1981";
date_to_string(d2) = "January 1, 1982";
date_to_string(d3) = "June 15, 1981";
date_to_string(d4) = "August 1, 1981";
date_to_string(d5) = "July 1, 1981";
date_to_string(d6) = "July 15, 1981";

number_before_reaching_sum(10, [1,2,3,4,5,6,7,8,9,10]) = 3;
number_before_reaching_sum(15, [1,2,3,4,5,6,7,8,9,10]) = 4;
number_before_reaching_sum(14, [1,2,3,4,5,6,7,8,9,10]) = 4;
number_before_reaching_sum(16, [1,2,3,4,5,6,7,8,9,10]) = 5;
number_before_reaching_sum(20, [1,2,3,4,5,6,7,8,9,10]) = 5;
number_before_reaching_sum(50, [1,2,3,4,5,6,7,8,9,10]) = 9;

what_month(1) = 1;
what_month(32) = 2;
what_month(60) = 3;
what_month(91) = 4;
what_month(121) = 5;
what_month(152) = 6;
what_month(182) = 7;
what_month(213) = 8;
what_month(244) = 9;
what_month(274) = 10;
what_month(305) = 11;
what_month(335) = 12;

month_range(1, 274) = [1,2,3,4,5,6,7,8,9,10];
month_range(1, 32) = [1,2];
