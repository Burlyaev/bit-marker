open StrucAlloc
open MethChoice
open StrucAlloc
open Cir2bdd
open CirG
open IoType
open Lst
open Mvfs
open TimingGl

(*format of stat result about connection between mem cells in seq graph*)
type fanPoint_T=
	{
	  nameP:string; (*name of the memory cells*)
	  fanIn:int; (*number of its predicessors*)
	  fanOut:int (*number of its successors*)
	  }
(*function that calculates for each  mem cells fanPoint_T struct and returns the list of this info*)
let pointFan (cirBdd:cirBdd_T) (grStr: grL_T) = 
  let seqGr= grStr.seq in
  let vertL= seqGr.vGL in
  let fanInL= List.map (fun vert -> List.length(G.pred seqGr.gr vert))  vertL in
  let fanOutL= List.map (fun vert -> List.length(G.succ seqGr.gr vert))  vertL in
  let enum=enumF (List.length vertL) in
  let pointL= List.map (fun num-> 
    {
      nameP= G.V.label(List.nth vertL num);
      fanIn = List.nth fanInL num;
      fanOut= List.nth fanOutL num
    }  
  ) enum in
  let sorted= List.sort (fun a b -> b.fanOut - a.fanOut) pointL in
    sorted;;

      
(*sorts mem cells accrod2 fan-out- but returns only MVFS- others not needed*)
let pointMvsFOut (cirBdd:cirBdd_T) (grStr: grL_T) = 
    let pointFanL = pointFan cirBdd grStr in (*poits list for mem cells*)
    let mvsStr= minVS filename grStr in  (*MVFS*)
    let mvsNameL = List.map (fun v-> G.V.label v) mvsStr.mvfsL in (*MFS names*)
    infO#com "\n Found MFVS::\n"; 
 let mvfsStr= List.fold_left (fun str name-> str^name^"\n") "MVFS list::" mvsNameL in 
 infO#com mvfsStr ;

    let filtL= List.map  (*keep only MVFS*)
	    (fun name-> List.find (fun p-> p.nameP=name) pointFanL)
    mvsNameL in
    let sorted= List.sort (fun a b -> a.fanOut - b.fanOut) filtL in

 let mvfsStr= List.fold_left (fun str a-> str^a.nameP^" : "^(string_of_int a.fanOut)^"\n") "FanOut+MVFS list::" sorted in 
 infO#com mvfsStr ;
    sorted;;

(*what mem cell is more important for voter into: where I should intro first*)
let pointPolicy=
    match collP.memPrior with
    |FanOutP -> pointFan
    |MvfsOutP -> pointMvsFOut
    | _->infO#com "[ERR]pointPolicy";exit 1
;;