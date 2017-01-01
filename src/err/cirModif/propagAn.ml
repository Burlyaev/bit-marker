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
open StopCond



(* one voter removal procedure - Error Propagation Analysis with output pattern*)
let oneVotRemovEPA (cirBu:cirBu_T) (vot2ChL: string list) (votL: string list) =

    let priorL= cirBu.priorBu in (*prioprity points, sorted donwwards*)
    (*find new voter to delete and check FT property*)
    let vot2del =  List.find (fun prior ->  (*choose the most important voter that exists in voters2check*)
			  (List.exists(fun vot2ch-> vot2ch=prior.nameP) vot2ChL))
			priorL in
infO#com ("[oneVotRemovEPA] we delete : "^vot2del.nameP);
infO#tim ("[oneVotRemovEPA] delete :: "^vot2del.nameP);
    (* delete this voter from checking list forever *)
    let newVot2ChL= List.filter (fun vot-> vot<>vot2del.nameP) vot2ChL in
    (* delete this voter from voter list *)
    let newVotL= List.filter (fun vot-> vot<>vot2del.nameP) votL in 
        (* voter introduction   *)
    let votCirBdd= introVotF cirBu.cirBu newVotL in (*we intro voters in cir with no vot*)
    (* we choose corruption fault model- SEU or SET *)
    let errIntro ss = match collP.faultModel with 
		      | SEU -> seuInjG cirBu.cirBu  ss
		      | SET -> setInjG cirBu.cirBu cirBu.grBu ss
    in
    let fixStSp= cirBu.fixStB  in (*fixed point St Sp- no errors introduced yet*)
 
  
    let rec globIter accGlSS lstSS=
	let errSS = errIntro lstSS in (*corruption*)
	let afterKSS= cycleK votCirBdd ioAut collP.parK errSS in  (*execute K cycles*)
	let fixSSL= cycleC cirBu.cirBu ioAut (condFixP) afterKSS in (*calc fixed point where no voters - no outs read are contaminated*)
	let accSS= List.hd fixSSL in (*accumulated SS >K after an error*)
	let ifErrVot= errVotFind cirBu.cirBu  votL accSS in (*if mems with voter are contamin*)
	let ifErrOut= errOutFind cirBu.cirBu ioAut accSS in (*if outs when read are contamin*)
	let ifAnyProb = ((List.length ifErrVot)>0) || (ifErrOut.errOutE) in 
	let ifFixedGl= Bdd.is_included_in  accSS (accGlSS) in (*if it's already in global acc*)
	match ifFixedGl with
	|true -> ifAnyProb(*reached global fixed point*)  
	|false-> (*did't reach global fixed point*)
		  match ifAnyProb with 
		  |true -> true (*yes, there are problems with error propagation*)
		  |false-> globIter (Bdd.dor accGlSS accSS) (accSS) (*no err, no fixed- new run*)
    in

    let ifErrB=  globIter fixStSp fixStSp in (*true- if there is an errror*)
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
let votReducEPA (cirBddu:cirBu_T) =
    (*we start: intro all voters posssible- MVFS*)
    let allVotL = List.map(fun point-> point.nameP)  cirBddu.priorBu in
infO#tim ("[votReduc]All voters are introduced - voter minim starts");    
    (* recusive voter minimisation *)
    let rec minimVot (vot2Ch:string list) (votOL:string list) =
infO#tim ("Voters 2 Check left#::"^(string_of_int (List.length vot2Ch)));
infO#com ("Voters 2 Check left#::"^(string_of_int (List.length vot2Ch)));
	match vot2Ch with 
	| [] ->  votOL
	| _ ->   let tryMinim= oneVotRemovEPA cirBddu vot2Ch votOL in 
		  minimVot tryMinim.vot2ChR tryMinim.newVotR
	    
    in
    let minVotL= minimVot allVotL allVotL in
    minVotL;;