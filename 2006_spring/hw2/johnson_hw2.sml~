fun toStr l = List.foldl (fn((fv,v),acc)=> acc ^ "("^ fv ^","^ Bool.toString(v)^") ") "\n" l (*For debugging*)
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

fun setof ls = List.foldl (fn(x,acc)=> newmem (x,acc)) [] ls;
infix union
fun l1 union l2 = setof (l1 @ l2);

infix isect
fun l1 isect l2 = List.foldl (fn(x,acc)=> if (x mem l2) then x::acc else acc) [] l1
(*What is the main advantage of not declaring the argument types of these functions?
Polymorphism.
 *)
infix isect_recursive (*not tail recursive as after recursive call, operation 'cons' is still to be performed*)
fun l1 isect_recursive [] = []
  | [] isect_recursive l2 = []
  | (l::ls') isect_recursive l2 = if (l mem l2) then l :: (ls' isect_recursive l2) else (ls' isect_recursive l2)

infix isect_tail (*tail recursive as at recursive call, nothing else needs to be done*)
fun l1 isect_tail ([], acc) = acc
  | [] isect_tail (l2, acc) = acc
  | (l::ls') isect_tail (l2, acc) = if (l mem l2) then ls' isect_tail (l2, (l::acc)) else (ls' isect_tail (l2, acc))
											    
;
setof [1,2,3,2] = [3,2,1];
[1,2,3] union [2,3,4];
[1,2,3] isect [2,3,4];
[1,2,3] isect_recursive [2,3,4];
[1,2,3] isect_tail ([2,3,4], []);

(*Section two*)
datatype 'a expr =
         Const of bool
       | Var of 'a
       | Not of 'a expr
       | And of 'a expr * 'a expr
       | Or of 'a expr * 'a expr
exception UnboundVar

fun free_vars (Const b) = []
  | free_vars (Var a) = [a]
  | free_vars (Not e) = free_vars e
  | free_vars (And (e1,e2)) = free_vars e1 union free_vars e2
  | free_vars (Or (e1,e2)) = free_vars e1 union free_vars e2

fun eval (Const b) = b
  | eval (Var a) = raise UnboundVar
  | eval (Not e) = not (eval e)
  | eval (And (e1,e2)) = (fn(t1,t2) => t1 andalso t2)(eval e1, eval e2) (*This way enforces that both e1 and e2 have no free variables*)
  | eval (Or (e1,e2)) = (fn(t1,t2) => t1 orelse t2)(eval e1, eval e2)

fun bind1 fv v (Const b) = Const b
  | bind1 fv v (Var a) = if a=fv then Const v else Var a
  | bind1 fv v (Not e) = Not (bind1 fv v e)
  | bind1 fv v (And (e1,e2)) = And (bind1 fv v e1, bind1 fv v e2)
  | bind1 fv v (Or (e1,e2)) = Or (bind1 fv v e1, bind1 fv v e2)

fun bind binder (Const b) = Const b
  | bind binder (Var a) = binder a
  | bind binder (Not e) = Not (bind binder e)
  | bind binder (And (e1,e2)) = And (bind binder e1, bind binder e2)
  | bind binder (Or (e1,e2)) = Or (bind binder e1, bind binder e2)
				  
fun bindvar fv v e = bind (fn(a)=>if a=fv then Const v else Var a) e
fun changevar fv v e = bind (fn(a)=>if a=fv then Var v else Var a) e

(*Given a size of binary digits return all the possible binary combinations that such a size can occupy
Example: Given a space of binary digits of size 2, to return all the possible combinations that such a size of binary digits may occupy translates to the following workout:
Size 2 => _ _ for binary digits 1 & 0 should yield following 4 possibilities: 1 1 - 0 0 - 1 0 - 0 1
The way of obtaining all these possibilities is by realizing that the above possibilities can be written as:
1 1
1 0
0 1
0 0
This gives us two columns that can be easily constructed: Column 1 = 1 1 0 0 and Column 2 = 1 0 1 0
In similar manner for size three we have following possibilities:

1 1 0
1 1 1
1 0 0
1 0 1
0 1 0
0 1 1
0 0 0
0 0 1

First column: 1 1 1 1 0 0 0 0
Second colum: 1 1 0 0 1 1 0 0
Third column: 1 0 1 0 1 0 1 0

The fun below constructs every column individually.
Once these three columns are obtained, they are then combined together, nth element of every list with the nth element of the other list in order so that the above example will result in:
[
 [1,1,1],
 [1,1,0],
 [1,0,1],
  ....
 [0,0,0]
] 

In our example, since we are dealing with size 3, we thus should in total have 2^3 amount of possibilities.
Originaly, my fun will produce like this:
1 1 1 1 0 0 0 0
1 1 0 0
1 0 

From this, we essentialy need:
1 1 1 1 
1 1
1
For the rest it suffices to mirror the zero to fill up accordingly.
Thereafter, we need to repeat our pattern  so that all the lists are of equal length:
1 1 1 1 0 0 0 0 is of length 8 and thus does not need to be extended
1 1 0 0         is only half the length so we need to extend it by doubling it so we get:        1 1 0 0 1 1 0 0
1 0             is only a fourth of the length so we need to add three more of itself so we get: 1 0 1 0 1 0 1 0

*)			    
fun binary_possibilities 0 bt bf = []
  | binary_possibilities n bt bf = 
    let val n = round(Math.pow(2.0,real(n)-1.0)) (*shadow n with the starting value*)
	fun build a 0 = []
	  | build a n = a :: build a (n-1)  (*Build a list of consecutive smth, e.g [1,1,1,1,1,..] or [true,true,true,...] etc.*)
	fun connect m 0 = []
	  | connect m i = build bt m @ build bf m @ connect m (i - 1) (*bring opposites together to form one list e.g. [1,1] and [0,0] should become [1,1,0,0]. Thereafter, repeat this pattern*)
	fun loop 0 = []
	  | loop i = [connect i (n div i)] @ loop (i div 2) 
	val enumerate = loop n
	val (amount_bin_possibilities, bin_enumerations) = List.foldl (fn(_,(i,acc))=> (i+1, (List.map (fn(l)=>List.nth(l,i)) enumerate)::acc)) (0,[]) (hd enumerate)
    in bin_enumerations
    end

fun satisfying_assignments_long e =
  let val fv_ls = free_vars e
      val lst_bin_enums = binary_possibilities (length fv_ls) true false
      val lst = List.map (fn(l)=> ListPair.zip (fv_ls, l)) lst_bin_enums
      val result = List.filter (fn(l)=> eval (List.foldl (fn((fv,v),e)=> bindvar fv v e) e l)) lst
  in result
  end

(*Improved version of above within ten lines only!
This shortened version works exactly in the opposite direction as the function binary_possibilities.
In fun bin_pos... we first start from big and create smaller subparts with a particular pattern  which we then repeat to grow them to the required length.
So 
1 1 1 1 0 0 0 0 was our starting point
Then 
1 1 0 0 was the second step which is a pattern we repeat one more times to make it gorw to the size of the first one so it becomes:
1 1 0 0 1 1 0 0
Likewise for the final step: 1 0 => 1 0 1 0 1 0 1 0

In this function I go exactly the opposite way, namely I start with the smalest instance and grow accordingly as is required.
The fun will first find out all the free variables, say x y and z
We traverse the list x,y,z one at a time, starting with x
Since there is only one variable, 'x' that we are dealing with, we thus can only subscribe to it 2 possibilities: (x,true) and (x,false)
We continue down the list and deal with the extra complexity of y as follows:
In our existing list add (y,true) so we get: 
[(x,true),(y,true)]
[(x,false),(y,true)]
Thereafter, on the same original list e.g. (x,true) and (x,false) we now have to repeat the procedure but then for (y,false), so we we get:
[(x,true),(y,false)]
[(x,false),(y,false)]

Thereafter combine both together as our new list. Then proceed to the next element z and increase the complexity of our existing list analogous to what we did for y.

After all the various possibilities have been enumerated, we now have our ultimate list, enumeration_lst. 
Filter out from that list, all the combinations for which an eval after their corresponding bindings have been made will evaluate to true.
*)
fun satisfying_assignments e =
  let val fv_ls = free_vars e
      fun build_possibilities [] acc = acc
	| build_possibilities (l::ls') acc =
	  let fun grow b = List.map (fn(el)=> (l,b) :: el) acc
	  in build_possibilities ls' (grow true @ grow false)
	  end
      val enl as enumerations_lst = build_possibilities fv_ls [[]]
  in List.filter (fn(bl as binding_lst)=> eval (List.foldl (fn((fv,v),e)=> bindvar fv v e) e bl)) enl
  end


;
val t1 = free_vars (And(Not(Var "x"),Or(Const true, Var "x")));
val t2 = eval (Or(Const true,Var "x")) = false handle UnboundVar=> true;
val t3 = eval (Or(Var "x",Const true)) = false handle UnboundVar=> true;
val t4 = bind1 "x" true (And(Var "x",Or(Var "y", Var "x")));
val t5 = bind1 "x" true (And(Var "z",Or(Var "y", Var "z")));
val t6 = bindvar "x" true (And(Var "x",Or(Var "y", Var "x")));
val t7 = changevar "x" "help" (And(Var "x",Or(Var "y", Var "x")));

val t8 = satisfying_assignments_long (Or(Var "x", Var "y"));
val t9 = satisfying_assignments_long (Or(Var "x", And(Var "y", Var "z")));
val t10 = satisfying_assignments_long (Or(Var "x", Or(Var "y", Or(Var "z", Or(Var "a", Or (Var "b", Or (Var "c", Var "d")))))));
val t11 = satisfying_assignments (Or(Var "x", Var "y"));
val t12 = satisfying_assignments (Or(Var "x", And(Var "y", Var "z")));
val t13 = satisfying_assignments (Or(Var "x", Or(Var "y", Or(Var "z", Or(Var "a", Or (Var "b", Or (Var "c", Var "d")))))));
