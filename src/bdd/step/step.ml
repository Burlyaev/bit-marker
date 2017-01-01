open Cudd
open Format
open BddTr
open TimingGl
open StrucAlloc

(*transtion function that updates the memory cells and IO-automaton state*)
let step (cirBdd:cirBdd_T) stSp=
 (*base current variable*)
  let inpVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.insVar) in
  let memVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.memVar) in
  let stVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.statVar) in
  let outVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.outVar) in

  let supp = List.fold_left Bdd.dand (Bdd.dtrue cirBdd.manBdd) 
		(inpVL @ memVL @ stVL @outVL) in (*support of all var*)
 (*primary- free variables*)
  let inpPrimVL= List.concat (List.map (fun vBdd-> vBdd.frVL) cirBdd.insVar) in
  let memPrimVL= List.concat (List.map (fun vBdd-> vBdd.frVL) cirBdd.memVar) in
  let stPrimVL= List.concat (List.map (fun vBdd-> vBdd.frVL) cirBdd.statVar) in
  let outPrimVL= List.concat (List.map (fun vBdd-> vBdd.frVL) cirBdd.outVar) in

  let tableInp = oneOther inpPrimVL inpVL in (*[[mem'-mem]]- transition*)
  let tableMem = oneOther memPrimVL memVL in (*[[inp-inp]] only limitat*)
  let tableSt  = oneOther stPrimVL  stVL in (*[[st-st]] only limitat*)
  let tableOut = oneOther outPrimVL outVL in (*[[mid'-mid]]*)

  let tableFull= tableInp @ tableMem @ tableSt @ tableOut in (*all pairs*)
  
  (* next state calculation *)
 
 (*nxt value variables*)
  let iniNxtVL = List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.insVar) in  (*cop inp*)
  let memNxtmVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.nxtMVar) in
  let stNxtVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.statVar) in 
  (*so nxt value of stat var is their prev value*)
  let outNxtVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.nxtOVar) in


(*equations: free var equals to the next value*)
  let iniEqL = xorFL inpPrimVL iniNxtVL in (*trans.fun.part for mem*)
  let memEqL = xorFL memPrimVL memNxtmVL in (*trans.fun.part for mem*)
  let stEqL = xorFL stPrimVL stNxtVL in (*state vars don't change*)
  let outEqL = xorFL outPrimVL outNxtVL in (*trans.fun.part  for outs*)
(*mem.trans intersect with SS*)
  let isecIniL= List.map ( fun eq -> Bdd.dand eq  stSp) iniEqL 
  in
  let isecMemL= List.map ( fun eq -> Bdd.dand eq  stSp) memEqL 
  in 
  let isecStL= List.map ( fun eq -> Bdd.dand eq  stSp) stEqL (*nextSt=prevSt*)
  in 
  let isecOutL= List.map ( fun eq -> Bdd.dand eq  stSp) outEqL 
  in 

(* intersection of intersections SSs   *)
  let intersect = List.fold_left Bdd.dand  
		      (Bdd.dtrue cirBdd.manBdd) (isecIniL@isecMemL@isecStL@isecOutL) 
  in

  let afterQuantL = Bdd.exist supp intersect in (*existential quantific.*)
    
  let renamedNEWL= Bdd.vectorcompose
		      (Array.of_list tableFull) 
		   afterQuantL
  in (*renaming operation- substitution*)
(*   infO#tim  ("\t\t: Image Comp End "); *)
  renamedNEWL;;