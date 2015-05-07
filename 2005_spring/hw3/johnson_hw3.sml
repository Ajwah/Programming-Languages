
datatype 'a expr =
         Const of bool
       | Var of 'a
       | Not of 'a expr
       | And of 'a expr * 'a expr
       | Or of 'a expr * 'a expr
exception UnboundVar
     
infix mem
fun x mem [] = false
  | x mem (y::ys) = x=y orelse x mem ys
fun newmem(x,xs) = if (x mem xs) then xs else x::xs
						     
fun setof ls = List.foldl (fn(x,acc)=> newmem(x,acc)) [] ls

fun free_vars expr =
  let fun helper expr =
      case expr of
	  Const _ => []
	| Var x => [x]
	| Not e => helper e
	| And (e1,e2) => (helper e1)@(helper e2)
	| Or (e1,e2) => (helper e1)@(helper e2)
  in setof (helper expr)
  end;
free_vars (And(Not(Var "x"),Or(Const true, Var "x"))) = ["x"];

fun eval expr =
  (*These two helper functions will enforce evaluation of all arguments so that invalid arguments are not shortcircuited from raising exception*)
  let fun helperOr e1 e2 = e1 orelse e2 
      fun helperAnd e1 e2 = e1 andalso e2
  in
      case expr of
	  Const b => b
	| Var x => raise UnboundVar
	| Not e => not (eval e)
	| And (e1,e2) => helperAnd (eval e1) (eval e2)
	| Or (e1,e2) => helperOr (eval e1) (eval e2)
  end;	 
eval (Or(Const true,Var "x")) handle UnboundVar => true;

fun bind1 v b e =
  case e of
      Const bo => Const bo
    | Var x => if x = v then Const b else Var x
    | Not e => Not (bind1 v b e)
    | And (e1,e2) => And (bind1 v b e1, bind1 v b e2)
    | Or (e1,e2) => Or (bind1 v b e1, bind1 v b e2);
(bind1 "x" true (And(Var "x",Or(Var "y", Var "x")))) = And(Const true,Or(Var "y", Const true));
(bind1 "x" true (And(Var "z",Or(Var "y", Var "z")))) = And(Var "z",Or(Var "y", Var "z"));

fun bind f e =
  case e of
      Const bo => Const bo
    | Var x => f x
    | Not e => Not (bind f e)
    | And (e1,e2) => And (bind f e1, bind f e2)
    | Or (e1,e2) => Or (bind f e1, bind f e2);

fun bindvar v b e = bind (fn(x)=>if x = v then Const b else Var x) e
fun changevar v b e = bind (fn(x)=> if x = v then Var b else Var x) e;
(bindvar "x" true (And(Var "x",Or(Var "y", Var "x")))) = And(Const true,Or(Var "y", Const true));
(changevar "x" "help" (And(Var "x",Or(Var "y", Var "x")))) = (And(Var "help",Or(Var "y", Var "help")));

fun satisfying_assignments e =
  let fun helper f e l r =
	case f of
	    [] => (l,r,(List.map (fn(x)=> eval x handle UnboundVar => false) e))
	  | f::f' => helper f' ((List.foldl (fn(x,acc)=> (bindvar f true x)::(bindvar f false x)::acc) [] e)@e) ((f,true,e)::l) ((f,false,e)::r)
  in helper (free_vars e) [e] [] []
  end;
