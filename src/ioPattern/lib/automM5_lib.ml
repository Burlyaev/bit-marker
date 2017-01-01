
(* output pattern *)
let b08M5_IO_NO =
[
((["TTTTTTTTT TTTTTTTTTTTTTTTTTTTTT"],["0000"]),(0,1)); (*unknown state*)
		    ((["10TTTTTTTTT"],["0000"]),(1,2)); (*reset*)
		    ((["010TTTTTTTT"],["0000"]),(2,3));  (*start_st- LOOPED*)
		    ((["011TTTTTTTT"],["0000"]),(3,4)); (*start- choice to start*)
		    ((["010TTTTTTTT"],["0000"]),(4,5)); (*init*)
		    ((["010TTTTTTTT"],["0000"]),(5,6)); (*loop_st*)
		    ((["010TTTTTTTT"],["0000"]),(6,7)); (*end: MAR 0->1*)
		    ((["010TTTTTTTT"],["0000"]),(7,8)); (*loop_st*)
		    ((["010TTTTTTTT"],["0000"]),(8,9)); (*end: MAR 1->2*)
		    ((["010TTTTTTTT"],["0000"]),(9,10)); (*loop_st*)
		    ((["010TTTTTTTT"],["0000"]),(10,11)); (*end: MAR 2->3*)
			((["010TTTTTTTT"],["0000"]),(11,12)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(12,13)); (*end: MAR 3->4*)
			((["010TTTTTTTT"],["0000"]),(13,14)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(14,15)); (*end: MAR 4->5*)
			((["010TTTTTTTT"],["0000"]),(15,16)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(16,17)); (*end: MAR 5->6*)
			((["010TTTTTTTT"],["0000"]),(17,18)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(18,19)); (*end: MAR 6->7*)
			((["010TTTTTTTT"],["0000"]),(19,20)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(20,21)); (*end: the TOTAL END*)
			((["010TTTTTTTT"],["1111"]),(21,2));		(*output reading step- 1 delay; O=reg*)	
			(*((["010TTTTTTTT"],["1111"]),(22,2))*)
		  ];;
(* output pattern *)
let b08M5_IO =
[
((["TTTTTTTTT TTTTTTTTTTTTTTTTTTTTT"],["0000"]),(0,1)); (*unknown state*)
		    ((["10TTTTTTTTT"],["0000"]),(1,2)); (*reset*)
		    ((["010TTTTTTTT"],["0000"]),(2,3));  (*start_st- LOOPED*)
		    ((["010TTTTTTTT"],["0000"]),(2,2));  (*BACK LOOP*)
		    ((["011TTTTTTTT"],["0000"]),(3,4)); (*start- choice to start*)
		    ((["010TTTTTTTT"],["0000"]),(4,5)); (*init*)
		    ((["010TTTTTTTT"],["0000"]),(5,6)); (*loop_st*)
		    ((["010TTTTTTTT"],["0000"]),(6,7)); (*end: MAR 0->1*)
		    ((["010TTTTTTTT"],["0000"]),(7,8)); (*loop_st*)
		    ((["010TTTTTTTT"],["0000"]),(8,9)); (*end: MAR 1->2*)
		    ((["010TTTTTTTT"],["0000"]),(9,10)); (*loop_st*)
		    ((["010TTTTTTTT"],["0000"]),(10,11)); (*end: MAR 2->3*)
			((["010TTTTTTTT"],["0000"]),(11,12)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(12,13)); (*end: MAR 3->4*)
			((["010TTTTTTTT"],["0000"]),(13,14)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(14,15)); (*end: MAR 4->5*)
			((["010TTTTTTTT"],["0000"]),(15,16)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(16,17)); (*end: MAR 5->6*)
			((["010TTTTTTTT"],["0000"]),(17,18)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(18,19)); (*end: MAR 6->7*)
			((["010TTTTTTTT"],["0000"]),(19,20)); (*loop_st*)
			((["010TTTTTTTT"],["0000"]),(20,21)); (*end: the TOTAL END*)
			((["010TTTTTTTT"],["1111"]),(21,2));		(*output reading step- 1 delay; O=reg*)	
			(*((["010TTTTTTTT"],["1111"]),(22,2))*)
		  ];;



(*b10  *)

(*let b10M5_IO =[
((["TTTTTTTTTTT TTTTTTTTTTTTTTTTT"],["000000"]),(0,1));
		  ((["10TTTTTTTTT"],["000000"]),(1,2));
		  ((["01TTTTTTTTT"],["111111"),(2,2))
		  ];;*)