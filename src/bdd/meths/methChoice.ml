open CirG
open StrucAlloc
open TimingGl
open LogOpM1
open LogOpM2
open LogOpM16
open Format
open Cudd

type gate2Op_T= vertLab -> vBDD_T -> vBDD_T -> vBDD_T;;
type gate1Op_T= vertLab ->  vBDD_T -> vBDD_T;;

type logOper_T =
    {
      notOp: gate1Op_T;
      dffOp: gate1Op_T;
      andOp:gate2Op_T;
      orOp: gate2Op_T;
      votOp: vBDD_T -> vBDD_T
    };;

(*casual 1 (Not, Dff) and 2 input (and, or) input funs *)
let logOps  =
  let meth= collP.meth in
  let and_Oper=
	match meth with 
	| TB -> m1AND
	| Precise -> m2AND
	| Bits16 -> m3AND
  in
  let or_Oper=
	match meth with 
	| TB -> m1OR
	| Precise -> m2OR	
	| Bits16 -> m3OR
  in
  let not_Oper=
	match meth with 
	| TB -> m1NOT
	| Precise -> m2NOT
	| Bits16 -> m3NOT
  in
  let dff_Oper=
	match meth with 
	| TB -> m1DFFX
	| Precise -> m2DFFX
	| Bits16 -> m3DFFX	
  in
  let vot_Oper=
	match meth with 
	| TB -> m1Vot
	| Precise -> m2Vot
	| Bits16 -> m3Vot	
  in
    {
      notOp=not_Oper;
      dffOp=dff_Oper;
      andOp=and_Oper;
      orOp=or_Oper ;
      votOp=vot_Oper
      };;


type funOp_T= AndF | NandF |OrF |NorF | NotF |DffF;;

let str2OpT (opS:string)= 
    match opS with
    |"AND" -> AndF
    |"NAND" -> NandF
    |"OR" -> OrF
    |"NOR" -> NorF
    |"NOT" -> NotF	
    |"DFFX" -> DffF
    |"INPUT" -> DffF
    |"OUTPUT" -> DffF
    |_-> infO#com "[ERR]str2OpT";exit 1
;;	



let logFun (var:vertLab) (argL: vBDD_T list)=
    let oper= str2OpT (var.opV) in
    let bddRes= 
      match (List.length argL) with 
      |1 -> (*operation with 1 argument: only not or dff- and outputs*)
	    (
	      match (oper,var.roleV) with 	
	      | (NotF,_) -> logOps.notOp var (List.hd argL)
	      | (DffF,_) -> logOps.dffOp var (List.hd argL)(*mem cells are just logically wires*)
	      | (_,Out) -> logOps.dffOp var (List.hd argL) (*outputs are just logically wires*)
	      | _ -> infO#com "[ERR]logFun";exit 1
	      )
      (*operation with more than 1 arg-> the rest of operations*)
      |_ ->  ( 
	      match oper with 	
	      | AndF -> List.fold_left 
			  (fun acc arg-> logOps.andOp var acc arg) 
			(List.hd argL) (List.tl argL)
	      | NandF ->
		logOps.notOp var (
				  List.fold_left 
				    (fun acc arg-> logOps.andOp var acc arg) 
				  (List.hd argL) (List.tl argL)
				  )
	      | OrF ->  List.fold_left 
			  (fun acc arg-> logOps.orOp var acc arg) 
			(List.hd argL) (List.tl argL)
	      | NorF ->  
		logOps.notOp var (
				  List.fold_left 
				    (fun acc arg-> logOps.orOp var acc arg) 
				  (List.hd argL) (List.tl argL)
				  )
	      | _ -> infO#com "[ERR]logFun";exit 1
	  )
    in
    { vname= var.nameV;
      vPL =[];
      vL=  bddRes.vL;
      frVPL =[]; frVL =[]; 
      vRole= match (oper,var.roleV)  with 
	      |(_, Inp) -> InpB  (*input vars*)
	      |(_, Out) -> OutB	(*out vars*)
	      |(DffF,Gate) -> MemB 	(*mem vars*)
	      |(_, Gate )-> GateB  	(*other logic.vars, not mem*)
     };;

(*translate level #2 - translation from sign to human representation*)
let transCube = 
    match collP.meth with
    | TB  	->  transM1
    | Precise 	->  transM2
    | Bits16    ->  transM3
;;
(*translate revers- translation from human to sign representation (and later to BDD possibly)*)
let transRevCube = 
    match collP.meth with
    | TB  	-> transRevM1
    | Precise 	-> transRevM2
    | Bits16 	-> transRevM3
;;

(* Error corruption *)

let seuInj (varBdd:vBDD_T) stSp=
  match collP.meth with
    | TB  	->  errInjM1 varBdd stSp
    | Precise 	->  errInjM2 varBdd stSp
    | Bits16 	->  errInjM3 varBdd stSp
;;

(*existance of err in varBdd in stSp-> returns bool*)
let checkErr (varBdd:vBDD_T) stSp=
    match collP.meth with
    | TB  	->  checkErrM1 varBdd stSp
    | Precise 	->  checkErrM2 varBdd stSp
    | Bits16 	->  checkErrM3 varBdd stSp
;;