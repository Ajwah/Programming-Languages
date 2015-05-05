(*Compiler.Control.Print.printDepth := 1000;
Compiler.Control.Print.printLength := 1000;
Compiler.Control.Print.stringDepth := 1000;
SMLofNJ.Internals.GC.messages false;
 *)
exception InsufficientArguments
exception NothingApplies

fun next l =
  let val convert_to_real = List.map (fn(x)=>real(x))
      val split = List.foldl (fn(e,(evens,unevens,i)) => if (i mod 2) = 0
							 then (evens@[e],unevens,i+1)
							 else (evens,unevens@[e],i+1))
			     ([],[],0)
      fun coupling l = ListPair.zip (#1 (split l), #2 (split l))
      fun couple_repeat (first::xs') =
	let val last::xs'' = List.rev xs'
	    val l = List.rev xs''
	in coupling (first::(List.foldl (fn(e,acc)=> acc@[e,e]) [] l)@[last])
	end
      fun round factor =
	let val sign = if factor < 0.0 then ~1.0 else 1.0
	    val fact = factor * sign
	in if fact > 0.0 andalso fact < 10.0 then ceil(100000.0 * factor)
	   else if fact > 10.0 andalso fact < 100.0 then ceil(10000.0 * factor)
	   else if fact > 100.0 andalso fact < 1000.0 then ceil(1000.0 * factor)
	   else ceil(100.0 * factor)
	end
      fun check_pattern f ((x as (_,last))::[]) = SOME (last, f x)
	| check_pattern f (e::cl') = if (List.all (fn(x)=>round(f(x))=round(f(e))
							  handle _ => false)
						  cl')
				     then check_pattern f [List.last cl']
				     else NONE
      fun arithmetic l = case (check_pattern (fn(x,y)=>y-x) l) of
			     NONE => NONE
			   | SOME (last, surplus)  => SOME (last + surplus)
      fun geometric l =	case (check_pattern (fn(x,y)=>y/x) l) of
			    NONE => NONE
			  | SOME (last, factor)  => SOME (last * factor)
      fun interleaved (e1::e2::[]) = NONE
	| interleaved (e1::e2::e3::[]) = NONE
	| interleaved l = 
	  let val evens = couple_repeat(#1 (split l))
	      val unevens = couple_repeat(#2 (split l))
	      val l_has_even = List.length(l) mod 2 = 0
	      fun retrieve a b = if l_has_even then SOME a else SOME b
	  in case (arithmetic evens, arithmetic unevens, geometric evens, geometric unevens) of
	         (SOME ne, SOME nu, _, _)=> retrieve ne nu
	       | (SOME ne, NONE, _, SOME nu) => retrieve ne nu
	       | (NONE, SOME nu, SOME ne, _) => retrieve ne nu
	       | (NONE,NONE,SOME ne, SOME nu) => retrieve ne nu
	       | (_,_,_,_) => NONE
	  end
  in case (convert_to_real l) of
	 [] => raise InsufficientArguments
       | e::[] => raise InsufficientArguments
       | l => case (arithmetic (couple_repeat l), geometric (couple_repeat l), interleaved l) of
		  (NONE,NONE,NONE) => raise NothingApplies
		| (SOME n,_,_) => ceil(n)
		| (NONE,SOME n,_) => ceil(n)
		| (NONE,NONE,SOME n) => ceil(n)
  end
	

fun gen(f, n) =
  let fun helper i =
	if i = n then [] else f(i)::(helper (i+1))
  in helper 0
  end;
   
