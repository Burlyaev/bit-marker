open TimingGl
open Cudd    
open Format
open StrucAlloc

let reorderF man iterat= 
  if (((iterat mod 10)=0)&&collP.reOrderFL) then 
	begin
	  infO#tim ("BDD build:Reorder::\t"^(string_of_int iterat));
	  Man.reduce_heap man  Man.REORDER_SIFT (0) 
	end;;

(*used during the building of transition function*)
let fixReordM1 man posStr=
  let inpPos= List.concat (List.map (fun invP -> invP.pL) posStr.insVP) in
  let memPos= List.concat (List.map (fun invP -> invP.pL) posStr.memVP) in
  let pairL= inpPos @ memPos in
  List.iter (fun pair ->Man.group man pair 2 Man.MTR_FIXED ) pairL
;;

(*after we allovated all variables: memory- automaton- outputs - inputs- we can do new fixation fo vars for further reordering*)
let reordFix (cirBdd:cirBdd_T) =
  let man=cirBdd.manBdd in

  let inpPos= List.concat (List.map (fun invP -> invP.vPL) cirBdd.insVar) in
  let memPos= List.concat (List.map (fun invP -> invP.vPL) cirBdd.memVar) in
  let autPos= List.concat (List.map (fun invP -> invP.vPL) cirBdd.statVar)in
  let othPos= List.concat (List.map (fun invP -> invP.vPL) cirBdd.outVar) in

  let pairL= inpPos @ memPos @ autPos @ othPos in
  List.iter (fun pair ->Man.group man pair 2 Man.MTR_FIXED ) pairL;;

(*just reordering function- after fixation necessary*)
let reorderSimpl man=
    if collP.reOrderFL then 
      Man.reduce_heap man Man.REORDER_SIFT 0
    else ignore 0;;


