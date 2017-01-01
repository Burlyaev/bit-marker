open Cudd
open StrucAlloc
open MethChoice
open StrucAlloc
open Cir2bdd
open CirType
open FanInOut
open TimingGl
open Model
open GenFun
open Contamin
open FormPairs


type votRemov_T= 
{
  errDetR:bool; (*if voter has been detected with newVotR voter list*)
  newVotR: string list;(*voter list*)
  vot2ChR: string list(*voters leftfor checking*)
}


(* function that intro voters to cirBdd structure and returns new cirBdd*)
let introVotF (cirBdd:cirBdd_T)  (votL: string list) =
  let withVoters= (*intro vot if needed*)
	  List.map 
	  (fun bdd -> 
	      match (List.exists (fun vot -> vot=bdd.vname) votL) with  
		      | true ->  logOps.votOp bdd(*we intro vot after this mem cell*)
		      | false -> bdd (*NO vot after this mem cell*)
	  ) 
	  cirBdd.nxtMVar in 
 {
    fileC=cirBdd.fileC; (*netlist file name*)
    manBdd=cirBdd.manBdd; (*BDD manager*)

    insVar=cirBdd.insVar;  (*base inputs*)
    memVar=cirBdd.memVar; (*base mem cells*)
    outVar=cirBdd.outVar; (*base outputs cells*)	
    statVar=cirBdd.statVar; (*IO automata state bits*)
    (*resolved nxt state*)
    nxtMVar=withVoters; (*resolved nxt val mem cells relatevely base vars*)
    nxtOVar=cirBdd.nxtOVar; (*resolved outs relatevely base vars*)
(*     nxtSVar=cirBdd.nxtSVar (*resolved automaton state variables*) *)
};;




