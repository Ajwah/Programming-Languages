Control.Print.printDepth := 99; (* deep structs for debugging. yes, := *)
infix mem
fun x mem [] = false
  | x mem (y::ys) = x=y orelse x mem ys
fun addmem x xs = if (x mem xs) then xs else x::xs
fun remmem x xs = List.foldl (fn(y,acc)=> if y=x then acc else y::acc) [] xs
fun setof xs = List.foldl (fn(x,acc)=>addmem x acc) [] xs
infix union
fun xs union ys = List.foldl (fn(x,acc)=> addmem x acc) (setof ys) xs
fun isect_1 [] ys = []
  | isect_1 xs [] = []
  | isect_1 (x::xs') ys = if x mem ys then x :: isect_1 xs' ys else isect_1 xs' ys

fun isect2 xs ys = 
  let fun helper [] ys acc = acc
      | helper xs [] acc = acc
      | helper (x::xs') ys acc = if x mem ys then helper xs' ys (x::acc) else helper xs' ys acc
  in helper xs ys []
  end
      
fun isect_foldl xs ys =  List.foldl (fn(x,acc) => if x mem ys then x::acc else acc) [] xs
fun isect_filter xs ys = List.filter (fn(x)=> x mem ys) xs

val t1 = isect_1 [1,2,3] [2,3,4]; 
val t2 = isect2 [1,2,3] [2,3,4]; 
val t3 = isect_foldl [1,2,3] [2,3,4]; 
val t4 = isect_filter [1,2,3] [2,3,4]; 

datatype expr = Const of bool
	      | Var of string
	      | Not of expr
	      | And of expr * expr
	      | Or of expr * expr
	      | All of string * expr
	      | Exist of string * expr
				      
exception UnboundVar
	      
val T = Const true;
val F = Const false;
val x = Var "x";
val y = Var "y";
val z = Var "z";

fun free_vars e =
  let fun helper (Const _) = [""]
	| helper (Var v) = [v]
	| helper (Not e) = helper e
	| helper (And (e1,e2)) = helper e1 @ helper e2 
	| helper (Or (e1,e2)) = helper e1 @ helper e2 
	| helper (All (_,e)) = helper e
	| helper (Exist (_,e)) = helper e
  in setof (helper e)
  end

fun getenv x env = case List.find (fn(s,v)=> s=x) env of
		       NONE => raise UnboundVar
		     | SOME (s,v) => v

fun eval e env = case e of
		     Const b => b
		   | Var x => getenv x env
		   | Not e => not (eval e env)
		   | And (e1,e2) => (fn(t1,t2)=>t1 andalso t2)(eval e1 env,eval e2 env) 
		   | Or (e1,e2) => (fn(t1,t2)=>t1 orelse t2)(eval e1 env,eval e2 env)
		   | All (_,e) => eval e env
		   | Exist (_,e) => eval e env
					 
fun fix1 x b e = case e of
		     Const b => Const b
		   | Var y => if y=x then Const b else Var y
		   | Not e => Not (fix1 x b e)
		   | And (e1,e2) => And (fix1 x b e1,fix1 x b e2) 
		   | Or (e1,e2) => Or (fix1 x b e1, fix1 x b e2)
		   | All (s,e) => All (s,e)
		   | Exist (s,e) => Exist (s,e)

fun fix f e =
  let fun helper e a =
	case e of
	    Const b => Const b
	  | Var y => if a=NONE orelse (SOME y)<>a then f (Var y) else Var y
	  | Not e => Not (helper e a)
	  | And (e1,e2) => And (helper e1 a, helper e2 a) 
	  | Or (e1,e2) => Or (helper e1 a, helper e2 a)
	  | All (s,e) => All (s, helper e (SOME s))
	  | Exist (s,e) => Exist (s, helper e (SOME s))
  in helper e NONE
  end

fun fixvar s b e = fix (fn(Var x)=> if x=s then Const b else Var x) e
fun changevar s v e = fix (fn(Var x)=> if x=s then Var v else Var x) e
fun swapvar x y e = fix (fn(Var l)=> if l=x then Var y else if l=y then Var x else Var l) e
			
val t5 = free_vars (And(x,All("x",Or(x,Exist("y",Or(x,Or(y,Exist("x",Or(Not(x),z)))))))));
val t6 = getenv "z" [("x",true),("z",false),("z",true)] = false;
val t7 = eval (Or(Const true,Var "x")) [("x",true),("z",false),("z",true)] = true;
val t8 = eval (Or((Var "x"),Const false)) [("x",true),("z",false),("z",true)] = true;
val t9 = eval (Or(Const true,Var "x")) [("z",false),("z",true)] = false handle UnboundVar => true;
val t10 = eval (Or((Var "x"),Const true)) [("z",false),("z",true)] = false handle UnboundVar => true;
val t11 = fix1 "x" true (And(Var "x", Or(All("x", Var "x"), Var "x")));
val t12 = fix1 "x" true (And(Var "z", Or(Var "y", Var "z")));
val t13 = fixvar "x" true (And(Var "x",Or(All("x",And(Var "x",Var "y")), Var "x")));
val t14 = changevar "x" "help" (And(Var "x",Or(All("x",And(Var "x",Var "y")), Var "x")));
val t15 = swapvar "x" "y" (And(Var "x",Or(Var "x",Var "y")));
