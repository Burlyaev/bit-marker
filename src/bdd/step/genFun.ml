open Cudd
open Format
open Step
open SsManip
open StrucAlloc
open IoType
open InpAutom
open Lst
open StopCond
open IoAutom
open TimingGl
open FormPairs
open BddShrink

(*returns stSp after initialisation: with updeted inps and aut for transtion*)
let initF (cirBdd:cirBdd_T) (aut: ioType_T list)=
   (*base current variable*)
  let inpVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.insVar) in
  let memVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.memVar) in
  let stVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.statVar) in
  let outVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.outVar) in
  let ss = List.fold_left Bdd.dand (Bdd.dtrue cirBdd.manBdd) 
		(inpVL @ memVL @ stVL @outVL) in (*support of all var*)
  let ss1= cleanSS cirBdd OutB ss in	(*clean outputs: other are cleaned before ini*)
  let ss2= cleanSS cirBdd InpB ss1 in	(*clean outputs: other are cleaned before ini*)
  let ss3= iniMemF cirBdd aut ss2 in	(*ini mems*)
  let ss4= iniAutF cirBdd  ss3 in	(*ini IO-aut*)
  ss4;;



(*do one clock cycle simulation*)
let cycle (cirBdd:cirBdd_T) (aut: ioType_T list) stSp=
    reorderSimpl cirBdd.manBdd; (*REORDERING CALL if allowed*) 
    infO#tim "[cycle] ...:: ";

    let autSS=updAutF cirBdd aut stSp in(*update automaton*)
    let inpSS= updInpF cirBdd aut autSS  in (*update input acc2 IO-aut*)
    let newSS=  step cirBdd inpSS in (*update mem and out*)
    newSS;;


(*do K clock cycles simulation*)
let cycleK (cirBdd:cirBdd_T) (aut: ioType_T list) k stSp= (*k is num of cycle to run*)
    let cycL= enumF k in
    List.fold_left 
      (fun ss _-> begin
		      let newSS= cycle cirBdd aut ss in
		      bdd2file cirBdd newSS;
		    newSS
		    end
      ) 
    stSp cycL;;



  (*do sim until the condition is true;
  condition takes an argument (list of states)*)
  let  cycleC (cirBdd:cirBdd_T) (aut: ioType_T list) cond stSp=
  
    let rec cycRec  stSpL  =
      let accSS_old= List.nth  stSpL 0 in (*accumulated state space*)
      let stSt_old= List.nth  stSpL 2 in (*current SS*)

      let newSS= cycle cirBdd aut stSt_old  in(*new state space*)
      let accSS= Bdd.dor accSS_old newSS in  (*new accumulated SS*)
      let ssNewL=[accSS; stSt_old; newSS] in (*state space list*)

(*stat2file  ((meth2mult collP.meth)*(List.length(cirBdd.memVar)+
				List.length(cirBdd.insVar))) accSS;*)

      match (cond [accSS_old;stSt_old;newSS]) with 
      |false -> cycRec ssNewL (*continue*)
      |true ->  ssNewL(*stop*)
    in
    cycRec [(Bdd.dfalse cirBdd.manBdd); (Bdd.dfalse cirBdd.manBdd);stSp]  ;;

(* calculate Fixed point state space*)
let calcFixSS (cirBdd:cirBdd_T) (aut: ioType_T list)=
    let iniSS= initF cirBdd aut in
    let lstSS= cycleC cirBdd aut (condFixP) iniSS in
    let accumSS= List.hd lstSS in
    accumSS;;


(*let zeroSt (cirBdd:cirBdd_T)  (aut: ioType_T list)=
    let inpVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.insVar) in
    let memVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.memVar) in
    let stVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.statVar) in
    let outVL= List.concat (List.map (fun vBdd-> vBdd.vL) cirBdd.outVar) in
    let ss = List.fold_left Bdd.dand (Bdd.dtrue cirBdd.manBdd) 
		(inpVL @ memVL @ stVL @outVL) in (*support of all var*)
      let ss1= cleanSS cirBdd InpB ss in (*delete all mems*)
      let ss2= cleanSS cirBdd OutB ss1 in (*delete all mems*)
      let ss3= cleanSS cirBdd MemB ss2 in (*delete all mems*)
      let ss4= iniAutF cirBdd  ss3 in
      cycle cirBdd aut ss4;;*)
