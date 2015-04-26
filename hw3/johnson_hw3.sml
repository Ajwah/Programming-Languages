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
(*
fun longest_string_helper f x y ls = foldl f(x,y) "" ls
val longest_string3 = longest_string_helper(fn(x,y) => if size x > size y then x else y)
val longest_string4 = longest_string_helper(fn(x,y) => if size x >= size y then x else y)
*)

val longest_capitalized = longest_string1 o only_capitals

(*Important note for function below:
fun rev_string(s: string) = implode o rev o explode s 
will not work

http://stackoverflow.com/questions/12729640/why-is-there-type-mismatch-of-operator-and-operand
*)
fun rev_string(s) = (implode o rev o explode) s
(*					    
fun first_answer f ls = foldl(f(x))
fun all_answers
*)
					      
					 
(*
9.a The function g takes three parameters in curried format, the first two are functions and the last one is a variable binding of type pattern. The function g eventually evaluates to type int.
In essence, the function g computes the given pattern p according to the criteria of the two functions evaluating to a single value of type int.
*)

fun count_wildcards p = g (fn(_) => 1) (fn(_) => 0) p
			
fun count_wild_and_variable_lengths p = g (fn(_) => 1) (fn(s) => String.size(s)) p
