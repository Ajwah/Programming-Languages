
datatype 'a expr =
         Const of bool
       | Var of 'a
       | Not of 'a expr
       | And of 'a expr * 'a expr
       | Or of 'a expr * 'a expr
exception UnboundVar
(*	      
infix mem
fun x mem [] = false
  | x mem (y::ys) = x=y orelse x mem ys
fun newmem(x,xs) = if (x mem xs) then xs else x::xs
						     
fun setof ls = List.foldl (fn(x,acc)=> newmem(x,acc)) [] ls

fun free_vars expr =
  case expr of
      Const _ => []
    | Var x => newmem(x,acc)
    | Not e => free_vars e
    | And (e1,e2) => (free_vars e1)@(free_vars e2)
    | Or (e1,e2) => (free_vars e1)@(free_vars e2)
				      
  
*)
