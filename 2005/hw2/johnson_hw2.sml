type bone = int * int
		      
type hand = bone list
type deck = bone list
type layout = bone list

datatype move = PlayFirst of bone
              | PlayLeft of bone
              | PlayRight of bone
       | PassDraw

exception BadMove
	      
	     
	     
