open Cudd
open StrucAlloc
open MethChoice
open Format
open IoType
open SsManip
open InpAutom
open Lst

(* returns names of mem cells where err is detected in stSp *)
let errMemFind (cirBdd:cirBdd_T) stSp=
  let memL= cirBdd.memVar in
  let ifErrPL= List.map (fun mem-> (mem.vname, (checkErr mem stSp))) memL in
  let errPL= List.filter (fun p -> snd p) ifErrPL in
  List.map (fun p-> fst p) errPL;;




type outErr_T=
  {
    errOutE: bool; (*if there is an error- true; other-false*)
    pos2outLE: (int * (string list)) list (*(qut pos, out names where the propagation condition is true) list*)
    }


(* returns names of outputs - if there is an error there and at this moment this output is read out *)
let errOutFind (cirBdd:cirBdd_T) (aut: ioType_T list) stSp=
   let outL= cirBdd.outVar in (*list of all circuit outputs*)
   let ss1= cleanSS cirBdd InpB stSp in (*delete all inpts -not important*)
   let ss2= cleanSS cirBdd MemB ss1 in (*delete all mems -not important*)
  
   let autT= trans_AutF aut in (*translate edge notation to common sign notation*)
   let stStSepL= divStSp2Aut cirBdd.manBdd cirBdd.statVar aut ss2 in (*divide SS to aut states*)
   
    let outErr ss= (*returns pair list: (output name, if error:bool) list*)
	  let ifErrPL=List.map (fun out -> (out.vname, checkErr out ss)) outL in 
	  ifErrPL
    in
      
   let ch4autSt num = (*(num, list of out names where everything is bad)*)
	let corStSp= snd( List.find (fun p -> (fst p=num)) stStSepL  ) in (*state state after division that corresp to this state num*)
	let corrOutPatt= (*output pattern for this aut state number*)
	      let autState=List.find (fun autSt-> autSt.nowSt=num) autT in
	      autState.outLim
	in
	let outNumL=(enumF ( String.length corrOutPatt)) in (*postions in aut pattern or number of output*)
	let outErrL= outErr corStSp in (*(output name, if there is an error) list*)
	let out2errL_all=List.map  (*(outout name, if coorrupted and read:bool)list*)
			    (fun pos-> 
				let posLetter= String.sub corrOutPatt pos 1 in (*1 or 0 - care or don't care*)
				let ifErrP = List.nth outErrL pos in (*out, if err) list*)
				let readAndWrong= ((posLetter="1") && (snd ifErrP)) in (*error and read now*)
				(fst ifErrP, readAndWrong)
			    ) outNumL in (*go through all outs*)
	let out2ErrL = List.filter (fun p-> snd p) out2errL_all in (*(outout name, yes-coorrupted and read)list*)
	(num, List.map (fun p-> fst p) out2ErrL) (*(aut St num, list of out names where it's bad)*)
    in
    let err4autStL= List.map (fun p-> ch4autSt (fst p)) stStSepL in
    let errNumber= List.concat (List.map (fun p -> snd p) err4autStL) in
    {
      errOutE= (errNumber<>[]);
      pos2outLE=err4autStL
      }
;;
(* returns the names of memory cells with voters where error exists in stSp*)
let errVotFind (cirBdd:cirBdd_T) (votL: string list) stSp=
  let memVL = List.map (fun vot-> List.find (fun vBdd-> vBdd.vname=vot) cirBdd.memVar) votL in (*bdd vars for memory cells with voters*)
  let ifErrPL= List.map (fun mem-> (mem.vname, (checkErr mem stSp) )) memVL in
  let errPL= List.filter (fun p -> snd p) ifErrPL in
  List.map (fun p-> fst p) errPL;;
  


	  
