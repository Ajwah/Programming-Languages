fun lreduce f [x] = x
  | lreduce f (x::x'::xs') = lreduce f ((f( x, x'))::xs')

infix \>
fun f1 \> f2 = f2 o f1
fun foldl fl i =
  let fun helper [f] = f
	| helper (f::fs') = f \> helper fs'
  in helper fl i
  end

fun pascal n =
    let fun helper 1 _ = [[1]]
	  | helper 2 _ = [[1,1],[1]]
	  | helper i (acc as ((head::tail)::rest)) =
	    let val (current,_) = (List.foldl (fn (e,(acc,prev))=> (acc@[e+prev],e)) ([1],head) tail)
	    in if i <= n then helper (i+1) ((current@[1])::acc) else acc
	    end
    in List.rev(helper 3 (helper 2 []))
    end;
(*
List.map (fn(l)=> let val p = List.foldl (fn(x,acc)=> acc^" "^(Int.toString(x))) "\n " l in (print p) end )
	 (pascal 32);*)
exception BadLists
fun innerproduct l1 l2 =
  if length l1 <> length l2 then raise BadLists
  else List.foldl (fn((x,y),acc)=> x*y + acc) 0 (ListPair.zip(l1,l2))

fun innerproductTail (l::ls') [] _ = raise BadLists
  | innerproductTail [] (l::ls') _ = raise BadLists
  | innerproductTail [] [] acc = acc
  | innerproductTail (x::xs') (y::ys') acc = innerproductTail xs' ys' (x*y+acc)

fun partition ls g = let fun helper [] pass fail = (pass,fail)
			   | helper (l::ls') p f = if g l then helper ls' (l::p) f else helper ls' p (l::f)
		     in helper ls [] []
		     end

fun lessThan p = fn(x)=> x < p

datatype 'a tree = EmptyT | Tree of 'a * 'a tree * 'a tree
type Tree = int tree

fun sumTree t =
  case t of
      EmptyT => 0
    | Tree (v,left,right) => v + sumTree left + sumTree right
							
fun countNodes t =
  case t of
      EmptyT => 0
    | Tree(v,left,right) => 1 + countNodes left + countNodes right

							     
fun findMax t =
  let fun helper max t =
	case t of
	    EmptyT => max
	  | Tree (v, l, r) => let val m1 = helper (if v > max then v else max) l
			      in helper m1 r
			      end
  in SOME (helper 0 t)
  end;

val myTree = Tree(5, Tree(4, EmptyT, Tree(7, EmptyT, EmptyT)), Tree(8, Tree(11, EmptyT, EmptyT), EmptyT) );
sumTree(myTree);
countNodes(myTree);
findMax(myTree);
