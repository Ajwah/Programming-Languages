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

fun longest_string_helper f ls = List.foldl(fn(x,acc) => if f(size x, size acc) then x else acc) "" ls
val longest_string3 = longest_string_helper (fn(x,y) => x > y)
val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

(*Important note for function below:
fun rev_string(s: string) = implode o rev o explode s 
will not work

http://stackoverflow.com/questions/12729640/why-is-there-type-mismatch-of-operator-and-operand
*)
fun rev_string(s) = (implode o rev o explode) s
					      
					    
fun first_answer f ls =
  case ls of
      [] => raise NoAnswer
    | x::xs' => case f(x) of
		    SOME v => v
		  | NONE => first_answer f xs'
					 
fun all_answers f ls =
  let fun accumulator(ls,acc) =
	case ls of
	    [] => SOME acc
	  | x::xs' => case f(x) of
			  SOME v => accumulator(xs',acc@v)
			| NONE => NONE
  in accumulator(ls,[])
  end

					      
					 
(*
9.a The function g takes three parameters in curried format, the first two are functions and the last one is a variable binding of type pattern. The function g eventually evaluates to type int.
In essence, the function g computes the given pattern p according to the criteria of the two functions evaluating to a single value of type int.
*)

fun count_wildcards p = g (fn(_) => 1) (fn(_) => 0) p
			
fun count_wild_and_variable_lengths p = g (fn(_) => 1) (fn(s) => String.size(s)) p
fun count_some_var (s,p) = g (fn(_) => 0) (fn(v) => if v = s then 1 else 0) p
			   
fun check_pat p =
  let fun extract_Var p =
	case p of
	    Variable s => [s]
	  | TupleP ls => List.foldl(fn(p,acc) => acc@(extract_Var p)) [] ls
	  | ConstructorP(_,p) => extract_Var p
	  | _ => []
      fun no_duplicates ls =
	case ls of
	    [] => false
	  | x::[] => true
	  | x::xs' => if (List.exists (fn(y) => y = x) (xs')) then false else no_duplicates(xs')
  in no_duplicates(extract_Var p)
  end

fun match(v,p) =
  case(v,p) of
      (_,Wildcard) => SOME []
    | (v,Variable s) => SOME [(s,v)]
    | (Unit,UnitP) => SOME []
    | (Const _, ConstP _) => SOME []
    | (Tuple(vs), TupleP(ps)) => if List.length vs = List.length ps
			       then all_answers (match) (ListPair.zip(vs,ps))
			       else NONE
    | (Constructor(s2,v),ConstructorP(s1,p)) => if s1 = s2
						then match(v,p)
						else NONE
    | _ => NONE
								
fun first_match v lp = SOME (first_answer (match) (List.map(fn(x) => (v,x)) lp))
			handle NoAnswer => NONE
