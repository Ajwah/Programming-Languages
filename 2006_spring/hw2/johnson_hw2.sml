infix mem
fun x mem [] = false
  | x mem (y::ys) = x=y orelse x mem ys
fun newmem(x,xs) = if (x mem xs) then xs else x::xs

(* 
In a comment near these functions, answer the following three questions:
1) What is the result of newmem(2,[1,2])? [1,2]
2) Of newmem("apple" ,["orange","banana"])? ["apple","orange","banana"]
3) Describe in your own words what these functions do, using only one sentence for each function.
mem evaluates the presence of a given element in a given list
newmem will include a given element in a given list if and only if it is not already present therein.
*)

						     
