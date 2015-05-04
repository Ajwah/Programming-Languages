(*Polynomial Representation*)
fun polynomial [] = []
  | polynomial (r::[]) = [1.0, ~r]
  | polynomial lr = 
    let fun mult p r  = List.foldl (fn((c1,c2),acc) => acc@[c1+c2]) [] (ListPair.zip(p@[0.0], 0.0::(List.map(fn(x) => ~r*x) p)))
	fun loop p [] = p
	  | loop p (r::lr') = loop (mult p r) lr'  
	val r1::r2::lr'' = lr
    in loop (mult [1.0, ~r1] r2) lr''
    end;
	
polynomial [~2.0, 2.0];
polynomial [1.0, 1.0, 1.0];
polynomial [0.0, 1.0, 2.0, 3.0];

(*Rough integrals.*)
fun roughIntegral f a b n =
  let val width = (b-a)/n
      fun helper c =
	if c > b then 0.0 else (f c) + helper (c + width)
  in (helper a)*width
  end;

roughIntegral (fn(x)=>Math.sqrt(1.0-x*x)) 0.0 1.0 4.0;
roughIntegral (fn(x)=>Math.sqrt(1.0-x*x)) 0.0 1.0 10.0;
roughIntegral (fn(x)=>x) 10.0 20.0 10.0;
      
