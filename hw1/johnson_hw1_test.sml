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

is_older(d2,d1) = not true;
is_older(d3,d1) = not false;
is_older(d4,d1) = not true;				   
is_older(d5,d1) = not false;
is_older(d6,d1) = not true;
is_older(d7,d1) = not false;
is_older(d8,d1) = not false;


val ld = [d1, d2, d4, d5, d6, d8, d7, d3];
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

val rnd_dates = [
    (2004,04,14),
    (1993,09,05),
    (1654,07,27),
    (1709,05,27),
    (1786,07,23),
    (1692,05,13),
    (1777,01,11),
    (2006,04,01),
    (1721,08,07),
    (1856,05,30),
    (1705,01,25),
    (1699,02,19),
    (1734,08,04),
    (1986,11,24),
    (1947,06,10),
    (1973,10,19),
    (1811,03,07),
    (1929,06,09),
    (1991,08,08),
    (1996,08,16),
    (1930,03,02),
    (1862,07,14),
    (1895,04,22),
    (1890,09,22),
    (1662,04,25),
    (2015,10,01),
    (1759,12,19),
    (1747,09,11),
    (1900,07,23),
    (1731,03,08),
    (1845,01,12),
    (1825,05,18),
    (1946,11,26),
    (1950,11,23),
    (1946,03,06),
    (1894,01,07),
    (1687,09,01),
    (1840,12,13),
    (1769,12,04),
    (1729,04,25),
    (1675,11,05),
    (1917,12,25),
    (1680,12,27),
    (1838,07,22),
    (1744,06,02),
    (1683,11,20),
    (1823,05,30),
    (1898,11,18),
    (1881,05,15),
    (1826,08,07),
    (1702,03,14),
    (1836,01,03),
    (2009,02,01),
    (1776,10,01),
    (2005,02,23),
    (1976,08,15),
    (2001,09,02),
    (1765,02,01),
    (1809,02,13),
    (1823,04,29),
    (1965,03,29),
    (1898,03,05),
    (1683,08,20),
    (1884,04,09),
    (1917,07,18),
    (1910,11,04),
    (1743,03,22),
    (1681,09,26),
    (1898,07,11),
    (2002,05,22),
    (1986,12,10),
    (1996,03,22),
    (1981,10,31),
    (1851,08,27),
    (1768,11,14),
    (1831,11,28),
    (1829,07,14),
    (1966,10,02),
    (1702,11,24),
    (1959,12,01),
    (1970,12,25),
    (1653,04,04),
    (1884,12,09),
    (1891,12,03),
    (1942,04,14),
    (1811,07,13),
    (1975,02,14),
    (1793,11,18),
    (1964,02,29),
    (1932,08,25),
    (2002,07,21),
    (1834,02,05),
    (1994,09,12),
    (1811,08,13),
    (1751,07,03),
    (1746,10,31),
    (1833,11,05),
    (1742,10,12),
    (1658,06,14),
    (1709,07,23)
];

oldest(rnd_dates);

val rnd_years = [
    1948,
1890,
1880,
1982,
1998,
1700,
2002,
2001,
1986,
1863,
1822,
1879,
1892,
1995,
1764,
1855,
1953,
1933,
1922,
1774,
1993,
1851,
1910,
1665,
1707,
1847,
1773,
2001,
1697,
1859,
1990,
1782,
1846,
1985,
1689,
1998,
1701,
1744,
1971,
1988,
1994,
1977,
1933,
1885,
1975,
1995,
1667,
1983,
1998,
2010,
1748,
1717,
1911,
1861,
1836,
1858,
1983,
1766,
1797,
1985,
1790,
1857,
1815,
1657,
1917,
1670,
1653,
1840,
1781,
1975,
1975,
2004,
1904,
1730,
1842,
1739,
1753,
1941,
1958,
1810,
1943,
1895,
1833,
2013,
1948,
1884,
1878,
1987,
1996,
1736,
1871,
1798,
1768,
1978,
1650,
1987,
1898,
1965,
2009,
2012
];

oldest_test(rnd_years);

number_in_months_challenge(ld, lm1 @ lm2 @ lm3 @ lm4 @ lm5 @ lm6) = 8;
dates_in_months_challenge(ld, lm2 @ lm1 @ lm2 @ lm3 @ lm4 @ lm5 @ lm6 @ lm2) = [d2,d1,d5,d6,d8,d7,d4,d3];
