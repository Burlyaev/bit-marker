open Cudd
open StrucAlloc
open MethChoice
open Cir2bdd
open CirG
open IoType
open FanInOut
open TimingGl
open GenFun
open CirType
open VotSuppr
open FormPairs
open BddShrink



(* building the static structures *)
let cirBuild =
infO#tim "[cirBuild]Circuit building:: Start";

  let libStr = itc2libG filename in (*parced structure*)
  let grStr  = itc2Grs libStr in  (*graph sctructures*)

infO#tim "Circuit building:: Parced and Graphs built";

  (*no voters at the beginning to calculate state space*)
  let cirBdd= cir2bdd [] filename ioAut in  (*bdd representation building*)
infO#tim "[cirBuild] cirBdd built:: ";
reordFix cirBdd;
reorderSimpl cirBdd.manBdd;
infO#tim "[cirBuild] Reorder after cir structure built done:: ";

  (* states spaces   *)
  let iniStSp= initF cirBdd ioAut in (*initial state space*)

stat2file ((meth2mult collP.meth)*(List.length(cirBdd.memVar)+
				List.length(cirBdd.insVar)))  iniStSp;
bdd2file cirBdd iniStSp;
infO#tim "[cirBuild] Init state found:: ";

  let fixStSp= calcFixSS cirBdd ioAut in (*fixed point St Sp*)

infO#tim "[cirBuild] Fixed-point calculated:: ";
dFileV "fixSS";
bdd2file cirBdd fixStSp;
stat2file   ((meth2mult collP.meth)*(List.length(cirBdd.memVar)+
				List.length(cirBdd.insVar))) 
				      fixStSp;

  (* priority for voter introduction   *)
  let pointL= pointPolicy cirBdd grStr in
infO#tim "[cirBuild] Memory cells priorities for voters are found:: ";
  {
    cirBu = cirBdd;
    grBu =grStr;
    priorBu=pointL;
    iniStBu= iniStSp;
    fixStB= fixStSp
    }
;;


  
