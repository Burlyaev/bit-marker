(*b09-Serial to serial converter   *)	
(*
let b09M5_IO =[
	((["TTT TTTTTTTTTTTTTTTTTTTTTTTTTTTT"],["0"]),(0,1));
		  ((["10T"],["0"]),(1,2));
		  ((["01T"],["1"]),(2,2))
		  ];;

(*[TODO] include output pattern *)
(* b11 *)
let b11M1Pat =[(["TTTTTTTTT TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT"],(0,1));
		  (["10TTTTTTTT"],(1,2));
		  (["01TTTTTTT"],(2,2))
		  ];;

let b11M1PatRest =[(["TTTTTTTTT TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT"],(0,1));
		  (["10TTTTTTTT"],(1,2));
		  (["01TTTTTTT"],(2,2))
		  ];;


let b11M1PatRest =[(["TTTTTTTTT TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT"],(0,1));
		  (["10TTTTTTTT"],(1,2));
		  (["01TTTTTTT"],(2,3));
		  (["01TTTTTT0"],(3,4));
		  (["01TTTTTT1"],(4,5));
		  (["01TTTTTT1"],(5,6));
		  (["01TTTTTT1"],(6,7));
		  (["01TTTTTT1"],(7,8));
		  (["01TTTTTT1"],(8,9));
		  (["01TTTTTT1"],(9,10));
		  (["01TTTTTT1"],(10,11));
		  (["01TTTTTT1"],(11,12));
		  (["01TTTTTT1"],(12,13));
		  (["01TTTTTT1"],(13,14));
		  (["01TTTTTT1"],(14,15));
		  (["01TTTTTT1"],(15,16));
		  (["01TTTTTT1"],(16,17));
		  (["01TTTTTT1"],(17,2));
		*)  ];;