
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

fun create_bytes ls = 
  let val n = List.length ls
      val all = ceil(Math.pow(2.0,real(n))) - 1
      fun construct_units _ 0 _ = []
	| construct_units (v::vs') n t =
	  let val size = ceil(Math.pow(2.0,real(n-1)))
	      fun loop 0 v b = []
		| loop n v b = ((v,b),(bindvar v b))::(loop (n-1) v b)
	      fun unit v = (loop size v true)@(loop size v false)
	      fun dupl 0 = []
		| dupl n = (unit v)@(dupl (n-1))
	  in [(dupl t)]@(construct_units vs' (n-1) (t*2))
	  end
	      
      val units = construct_units ls n 1
      fun create_byte i = List.map (fn(l)=>List.nth(l,i)) units
      fun loop ~1 = []
	| loop n = (create_byte n)::(loop (n-1))
  in loop all
  end

fun satisfying_assignments e =
  let val vars = free_vars e
      val size = List.length vars
      val bindings = create_bytes vars
      fun partial_binding_expr ((b as binding,f),(acc,result)) = (acc@[b],(f result))
      fun apply_bindings ls =
	let val (a as applied_binding_summary,c as concrete_expr) = (List.foldl (partial_binding_expr)
										([],e)
										ls)
	in (a, eval c)
	end
      val enumeration_results = List.map (fn(ls) => apply_bindings ls) bindings
      val relevant_results = List.foldl (fn((a,b),acc)=> if b then acc@[a] else acc)
						 []
						 enumeration_results
  in
      relevant_results
  end;


satisfying_assignments (And(Not(Var "x"),Or(Const true, Var "x"))) = [[("x",false)]];
satisfying_assignments (And(Not(Var "x"),Or(Const false, Var "x"))) = nil;
satisfying_assignments (Or(Var "x", Var "y")) = [
    [("y",false),("x",true)],[("y",true),("x",false)],[("y",true),("x",true)]
];
satisfying_assignments (Const true) = [[]];
satisfying_assignments (Const false) = [];

(*PART II*)

datatype atom = AtomConst of string | AtomVar of string
type predicate = { pred : string, vals : atom list };
exception BadGetBinding
	      
val state = [ {pred="At", vals=[AtomConst "home"]},
              {pred="Sells",vals=[AtomConst "QFC", AtomConst "Eggs"]} ];

(*val buy_precond = And({pred="At", vals=[AtomVar "place"]},
                      {pred="Sells", vals=[AtomVar "place",AtomVar "item"]});
 *)
val x = AtomVar "x"; val y = AtomVar "y";
val QFC = AtomConst "QFC"; val Ace = AtomConst "Ace";
val Eggs = AtomConst "Eggs"; val Nails = AtomConst "Nails";
fun At(x) = {pred="At", vals=[x]};
fun Sells(x,y) = {pred="Sells",vals=[x,y]};

fun bindatom (x,c) (a as (AtomVar v)) = if x=v then AtomConst c else a
  | bindatom _ a = a

fun bindpred (s as (x,c)) {pred=p,vals=v} = List.map (fn(a)=>bindatom s a) v
fun filter pred [] = []
  | filter pred (l::ls') = if pred l then l::(filter pred ls') else (filter pred ls')
fun getconsts (AtomConst _) = true
  | getconsts _  = false 
fun getvars (AtomVar _) = true
  | getvars _  = false

fun satisfying_matches pred (state as []) = []
  | satisfying_matches (pred as {pred=p,vals=v}) ({pred=q,vals=w}::s') =
    let val is_pass_precheck = (p=q) andalso (List.length(v) = List.length(w))
	fun comp_vals (AtomVar _) _ = true
	  | comp_vals (AtomConst a) (AtomConst b) = a=b
	  | comp_vals _ _ = false
	fun get_value (AtomVar v) = v
	  | get_value (AtomConst v) = v 
	fun compare [] [] acc = acc
	  | compare (v::vs') (w::ws') acc =
	    if (comp_vals v w) then (compare vs' ws' (acc@[get_value w])) else [] 
    in if is_pass_precheck
       then (compare v w [])::(satisfying_matches pred s')
       else (satisfying_matches pred s')
    end;
							    
satisfying_matches {pred="Sells", vals=[AtomVar "x", AtomVar "y"]}
                   [{pred="Sells", vals=[AtomConst "QFC", AtomConst "Eggs"]},
		    {pred="Sells", vals=[AtomConst "Ace", AtomConst "Nails"]},
		    {pred="At", vals=[AtomConst "Home"]},
		    {pred="Sells", vals=[AtomConst "QFC", AtomConst "Milk"]}];
satisfying_matches (Sells(x,y)) [At(x),Sells(x,y), At(QFC),Sells(QFC,Eggs)];

fun get_binding [] [] = []
  | get_binding (x::xs') [] = raise BadGetBinding
  | get_binding [] (y::ys') = raise BadGetBinding				    
  | get_binding (x::xs') (y::ys') =
    case (x,y) of
	(AtomVar a, AtomConst b) => (a,b)::(get_binding xs' ys')
     |  (AtomVar a, AtomVar b) => (a,b)::(get_binding xs' ys')
     |  (AtomConst a, AtomConst b)=> if (a=b) then (get_binding xs' ys') else raise BadGetBinding
     |  _ => raise BadGetBinding;

get_binding [AtomVar "x", AtomVar "y"] [QFC,Eggs];
get_binding [QFC,AtomVar "y"] [QFC,Eggs];

		  
