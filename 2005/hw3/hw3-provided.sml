signature GRAPHER = sig
	val graph_window 
		:int * int -> 
		real * real ->
		real * real ->
		(real -> real)
		-> unit

	val graph
		:(real -> real) -> unit
end


structure Grapher :> GRAPHER = struct

fun make_columns (height, values, xaxis, yaxis) =
	let fun mc (x, dot) =
		let fun helper (pos) =
			case pos of
				  0 => []
				| _ =>	(if SOME pos = dot then #"o" else
						if x = yaxis andalso pos = xaxis then #"+" else
						if pos = xaxis then #"-" else
						if x = yaxis then #"|" else
						#" ")::helper(pos-1)
		in
			helper(height)
		end
		fun loop (x, vals) =
			case vals of
				  [] => []
				| y::rest => mc(x,y)::loop(x+1,rest)
	in loop(0, values) end

fun convert_to_strings (cols, number) =
	case number of
		  0 => []
		| _ => (implode (List.map hd cols) ^ "\n")
		       ::convert_to_strings(List.map tl cols, number-1)

fun deltas (width, height, xmin, xmax, ymin, ymax) =
	let val dx = (xmax-xmin)/(Real.fromInt width)
		val dy = (ymax-ymin)/(Real.fromInt height)
	in (dx,dy) end

fun toInt x =
	SOME (Real.toInt IEEEReal.TO_NEGINF x) handle
		overflow => NONE

fun create_values (f, width, height, xmin, xmax, ymin, ymax) =
	let val (dx,dy) = deltas(width,height,xmin,xmax,ymin,ymax)
		val dy = (ymax-ymin)/(Real.fromInt height)
		fun helper (pos, x) =
			if pos = width
			then []
			else toInt((f(x)-ymin)/dy+1.0)::helper(pos+1, x+dx)
	in helper(0, xmin) end

fun graph_draw (f, width, height, xmin, xmax, ymin, ymax) =
	let val (dx,dy) = deltas (width, height, xmin, xmax, ymin, ymax)
		val values = create_values (f,width,height,xmin,xmax,ymin,ymax)
		val xaxis = valOf (toInt ((0.0 - ymin)/dy + 1.0))
		val yaxis = valOf (toInt ((0.0 - xmin)/dx))
		val columns = make_columns (height, values, xaxis, yaxis);
		val strings = convert_to_strings (columns, height);
	in
		List.app print strings
	end

fun graph_window (width,height) (xmin,xmax) (ymin,ymax) f =
	graph_draw (f, width, height, xmin, xmax, ymin, ymax)

val graph = graph_window (68,29) (~10.0, 10.0) (~10.0, 10.0);

end
