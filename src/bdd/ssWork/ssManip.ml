(* Overloaded operators only for the loval use in this file *)
open Lst
open Cudd
open StrucAlloc
open TimingGl
(*********WORKING with State Space*)

(*all inputs cleaning in StSp- base vars and free vars*)


(*exustential quatification of all vars of choisen group vars2cl in state space stSp*)
let cleanSS  (cirBdd:cirBdd_T) (vars2cl:bddVRole_T) stSp=
  let varsL= 
	  match vars2cl with
	  | InpB -> cirBdd.insVar 
	  | OutB -> cirBdd.outVar 
	  | MemB -> cirBdd.memVar 
	  | AutB -> cirBdd.statVar 
	  | _  ->  infO#com "[ERR]cleanSS";exit 1
 in
  let bddVars= List.concat (List.map (fun inp -> inp.vL @ inp.frVL) varsL) in
  List.fold_right Bdd.exist bddVars stSp;;



(*restriction of stSp with var accord by ch specific*)
let restrSS bddV (ch:string) stSp=
  let setFunc=
	match ch with 
	| "+" -> Bdd.dand stSp bddV
	| "!" -> Bdd.dand stSp (Bdd.dnot bddV)
	| "D" -> Bdd.exist bddV stSp
	| _ -> infO#com "[ERR]restrSS";exit 1
  in
  setFunc;;


(*restriction stSp with var list accord to specif.pattern*)
let restrVarL bddVL (patt:string) stSp =
  List.fold_left
    (fun ss pos-> restrSS (List.nth bddVL pos) (String.sub patt pos 1) ss) 
  stSp (enumF (String.length patt))
;;



(*cleaning of all vars except mem.cells in StSp*)
(*let keepMems (cirBdd:cirBdd_T) stSp=
  let inpL= cirBdd.insVar in
  let freeL= cirBdd.freeVar in
  let bddVars= 
    List.concat 
      (List.map (fun inp -> inp.vL@inp.frVL) (inpL @ freeL)) in
  List.fold_right Bdd.exist bddVars stSp;;*)

(*buildSet-- func that build a state merging all variables through AND operator  *)
(* used no-where -commented out  *)
(*let buildStSp (cirBdd:cirBdd_T) (vL:v_T list)= 
  if ((List.length vL) > 0) then 
      List.fold_left Bdd.dand (List.hd vL) vL
  else
    Bdd.dfalse cirBdd.manBdd
  ;;*)

(* [TODO] where is used? Nowhere- commented out *)
(*negVinS-- func. supporting func for IniState;
      it takes the state space and var;
      this func. negates this var in the whole state space*)
(*let negVinS var stSp = 
  let vNum = 
    try
      List.hd ( Bdd.list_of_support var)  
    with _-> infO#com "[ERR]negVinS";exit 1
  in
  Bdd.compose vNum (Bdd.dnot var) stSp;;*)
