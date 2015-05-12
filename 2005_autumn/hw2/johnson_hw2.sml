datatype eval_let_type = SML | Scheme

val type_of_eval = SML
		       
datatype 'a expr =
	 Const of 'a
	 | Var of string
	 | Bind of string * 'a expr
	 | `~ of 'a expr
	 | `+ of 'a expr * 'a expr
	 | `- of 'a expr * 'a expr
	 | `* of 'a expr * 'a expr
	 | `/ of 'a expr * 'a expr
	 | Let of 'a expr list * 'a expr
				 
exception UnboundVariable
exception SyntaxError
			
fun lookup v en = case (List.find (fn(var,vl)=> v=var) en) of
		      NONE => raise UnboundVariable
		    | SOME (var,vl) => vl
					  
fun process_let e v_init vl_init en ls' =
  let val let_SML =  List.foldl (fn(Bind (v,vl),acc)=> (v,eval vl acc)::acc)
				((v_init, eval vl_init en)::en)
				ls'
      val let_Scheme = List.foldl (fn(Bind (v,vl),acc)=> acc@[(v,eval vl en)] handle UnboundVariable => acc@[(v,eval vl acc)])
				  [(v_init, eval vl_init en)]
				  ls'
  in case type_of_eval of
	 SML => eval e let_SML
       | Scheme => eval e (let_Scheme@en)
  end  
and eval ex en =
  case ex of
      Const a => a
    | Var a => lookup a en
    | `~a => ~(eval a en)
    | `+ (a,b) => (eval a en) + (eval b en)
    | `- (a,b) => (eval a en) - (eval b en)
    | `* (a,b) => (eval a en) * (eval b en)
    | `/ (a,b) => (eval a en) div (eval b en)
    | Let ([],_)  => raise SyntaxError
    | Let ((Bind (v_init,vl_init))::ls',e) => process_let e v_init vl_init en ls'
      
val e1 = Const 5;
val e2 = `+(Const 5, Const 3);
val e3 = `-(Const 5, Const 3);
val e4 = `*(Const 5, Const 3);
val e5 = `/(Const 4, Const 2);
val e7 = `+(e2,e3);
val e8 = `-(e2,e3);
val e9 = `*(e2,e3);
val e10 = `/(e2,e3);
val e11 = `*(e7,e4);

eval e1 [] = 5;
eval e2 [] = 8;
eval e3 [] = 2;
eval e4 [] = 15;
eval e5 [] = 2;
eval e7 [] = 10;
eval e8 [] = 6;
eval e9 [] = 16;
eval e10 [] = 4;
eval e11 [] = 150;

val e12 = "x";
val env1 = [("x",5), ("y",3)];
lookup e12 env1 = 5;

val e13 = Var "x";
val e14 = Var "y";

val e15 = Const 5;
val e16 = `+(e13, e14);
val e17 = `-(e13, e14);
val e18 = `*(e13, e14);
val e19 = `/(e13, e14);
val e21 = `+(e2,e3);
val e22 = `-(e2,e3);
val e23 = `*(e2,e3);
val e24 = `/(e2,e3);
val e25 = `*(e7,e4);
eval e15 [] = 5;
eval e16 env1 = 8;
eval e17 env1 = 2;
eval e18 env1 = 15;
eval e19 env1 = 1;
eval e21 env1 = 10;
eval e22 env1 = 6;
eval e23 env1 = 16;
eval e24 env1 = 4;
eval e25 env1 = 150;

val e30 = Let([Bind("x",Const(3)), Bind("y",`+(Var("x"),Const(1)))] ,`+(Var("x"),Var("y")));
val e31 = Let([Bind("x",Const(3)), Bind("y",`+(Var("x"),Const(10)))] ,`+(Var("x"),`+(Var("y"),Var("z"))));
val e32 = Let([Bind("x",Const(3)), Bind("y",`~ (Var("x")))] ,Let([Bind("x",Const (100))],`+(Var("x"),Var("y"))));
val e33 = Let([Bind("x",Const(3)), Bind("y",`+ (Var("x"), Const 1))] ,Let([Bind("x",`+(Var "x", Var "y")),Bind("y",`+(Var "x", Var "y"))],`+(Var("x"),Var("y"))));
val e34 = Let([Bind("x",Const(3))], Let([Bind("x",Const (100)), Bind("y", Var "x")],`+(Var("x"),Var("y"))));
val e35 = Let([Bind("x",Const(3))], Let([Bind("x",Const (100)), Bind("y", Var "x")], Let([Bind("x", Const 15),Bind("y", Const 5)],`+(Var("x"),Var("y")))));
val e36 = Let([Bind("x",Const(3))], Let([Bind("x",Const (100)), Bind("y", Var "x")], Let([Bind("x", Var "x"),Bind("y", Var "y")],`+(Var("x"),Var("y")))));


eval e30 [] = 7;
eval e31 [] = 0 handle UnboundVariable => true;
eval e32 [] = 97;
eval e33 [] =  (if type_of_eval = SML then 18 else 14);
eval e34 [] =  (if type_of_eval = SML then 200 else 103);
eval e35 [] =  (if type_of_eval = SML then 20 else 20);
eval e36 [] =  (if type_of_eval = SML then 200 else 103);
