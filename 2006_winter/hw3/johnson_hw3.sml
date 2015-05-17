fun lreduce f [x] = x
  | lreduce f (x::x'::xs') = lreduce f ((f( x, x'))::xs')
						 
