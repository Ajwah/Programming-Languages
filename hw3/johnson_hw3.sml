(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals(ls) = List.filter (fn(x) => case size x of
						  0 => false
						| _ => Char.isUpper(String.sub(x,0))) ls
				    
fun longest_string1(ls) = foldl(fn(x,y) => if size x > size y then x else y) "" ls
fun longest_string2(ls) = foldl(fn(x,y) => if size x >= size y then x else y) "" ls
fun longest_string_helper f(x,y) ls = foldl f(x,y) "" ls
val longest_string3 = longest_string_helper(fn(x,y) => if size x > size y then x else y)
val longest_string4 = longest_string_helper(fn(x,y) => if size x >= size y then x else y)
