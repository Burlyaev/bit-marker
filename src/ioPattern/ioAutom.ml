open Cudd
open IoType
open StrucAlloc
open Lst
open TimingGl
open SsManip
open Num2Bdd
open InpAutom

(*update automaton state based on already updated state of the state aut*)  
let updAutF (cirBdd:cirBdd_T) (aut: ioType_T list) stSp=
    let ss1= cleanSS cirBdd InpB stSp in (*delete all inpts for new restriction*)
    let autT= trans_AutF aut in (*translate edge notation to common sign notation*)
    let stStSepL= divStSp2Aut cirBdd.manBdd cirBdd.statVar aut ss1 in (*divide SS to aut states*)
    let stNumL= enumF (List.length stStSepL) in (*all state nums 0...n-1*)

    let nxtSLL = List.map ( fun nowSt->  (*for each curr state we fins a list of next states*)
			      List.map (fun str-> str.nxtSt)
				  (List.filter (fun st-> st.nowSt=nowSt) autT)
			    ) 
		stNumL  in (*return [[nxt1,nxt1];[nxt2];[nxt3,nxt3]]*)
      
    let num2BddM=num2BddMap cirBdd.manBdd cirBdd.statVar aut in

    let updateSt nxtStL curStSp =
	  let noAut= cleanSS cirBdd AutB curStSp in 
	  let toIntersPL= List.map 
		      (fun nxt-> 
			    List.find (fun p-> (fst p)=nxt) num2BddM) 
		      nxtStL in
	  let toIntersSSL= List.map (fun p-> snd p) toIntersPL in
	  let newAutL= List.map (fun interSS-> Bdd.dand noAut interSS ) toIntersSSL in
	  List.fold_left (Bdd.dor) (Bdd.dfalse cirBdd.manBdd) newAutL 
    in	
    
    let updEachStL =List.map2 
	    (fun stSpCur nxtL-> updateSt nxtL (snd stSpCur)) stStSepL nxtSLL in
    let merge = List.fold_left (Bdd.dor)(Bdd.dfalse cirBdd.manBdd) updEachStL in
    merge;;

  
  

 
	  
  

