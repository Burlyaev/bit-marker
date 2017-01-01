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
open VotSuppr



(* one voter removal procedure *)
let oneVotRemovEAA (cirBu:cirBu_T) (vot2ChL: string list) (votL: string list) =

    let priorL= cirBu.priorBu in (*prioprity points, sorted dowwards*)
    (*find new voter to delete and check FT property*)
    let vot2del =  List.find (fun prior ->  (*choose the most important voter*)
			  (List.exists(fun vot2ch-> vot2ch=prior.nameP) vot2ChL))
			priorL in
infO#com ("[oneVotRemovEAA] we delete : "^vot2del.nameP);
infO#tim ("[oneVotRemovEAA] delete :: "^vot2del.nameP);
    (* delete this voter from checking list forever *)
    let newVot2ChL= List.filter (fun vot-> vot<>vot2del.nameP) vot2ChL in
    (* delete this voter from voter list *)
    let newVotL= List.filter (fun vot-> vot<>vot2del.nameP) votL in 
    
    let fixStSp= cirBu.fixStB  in (*fixed point St Sp*)
    let errStSp= match collP.faultModel with (*we corrupt this fixed point*)
		  | SEU -> seuInjG cirBu.cirBu  fixStSp
		  | SET -> setInjG cirBu.cirBu cirBu.grBu fixStSp
    in
fDataF ("rem_"^vot2del.nameP); (*ranaming of output data file*)
bdd2file cirBu.cirBu errStSp;
dFileV "execut";
    (* voter introduction   *)
    let votCirBdd= introVotF cirBu.cirBu newVotL in (*we intro voters in cir with no vot*)
    let stSpC= cycleK votCirBdd ioAut collP.parK errStSp in  (*execute K cycles*)
    (*  check if error is there *)
    let ifErrL= errMemFind cirBu.cirBu stSpC in
infO#com ("[oneVotRemovEAA] we detecte #errors: "^(string_of_int (List.length ifErrL)));
infO#tim ("[oneVotRemovEAA] checked :: "^vot2del.nameP);
    let ifErrB= (List.length ifErrL) >0 in (*true- if there is an errror*)
    let decVotL= match ifErrB with 
		  | true -> votL(*there is an error; take original voter list*)
		  | false -> newVotL(*there is no errors; take reduced voter list*)
     in
    {	
      errDetR= ifErrB;
      newVotR= decVotL;
      vot2ChR= newVot2ChL
      };;


(* recursive voters reduction iterations  - for EAA*)
let votReducEAA (cirBddu:cirBu_T) =
    (*we start: intro all voters posssible*)
    let allVotL = List.map(fun point-> point.nameP)  cirBddu.priorBu in
infO#tim ("[votReduc]All voters are introduced - voter minim starts");    
    (* recusive voter minimisation *)
    let rec minimVot (vot2Ch:string list) (votOL:string list) =
infO#tim ("Voters 2 Check left#::"^(string_of_int (List.length vot2Ch)));
infO#com ("Voters 2 Check left#::"^(string_of_int (List.length vot2Ch)));
	match vot2Ch with 
	| [] ->  votOL
	| _ ->   let tryMinim= oneVotRemovEAA cirBddu vot2Ch votOL in 
		  minimVot tryMinim.vot2ChR tryMinim.newVotR
	    
    in
    let minVotL= minimVot allVotL allVotL in
    minVotL;;
    