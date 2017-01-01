open Cudd
open TimingGl
open StrucAlloc
open VarResolv
open CirG
open BddShrink
open MethChoice
open IoType

open Lst
open SsManip
open Num2Bdd

(*Function translates IO-aut to sign level; str list format is kept*)
  let trans_AutF (aut: ioType_T list)=
    List.map
	(fun ioStr-> 
	      let len= String.length ioStr.iLim in (*len of inp spec- number of inp*)
	      let lenM= String.length ioStr.stLim in (*number of mem- mem spec*)
	      {iLim= String.concat "" (
		      List.map
			  ( fun pos-> let letter= String.sub ioStr.iLim pos 1 in
				      transRevCube letter
			  ) 
			(enumF len) 
		      );
	       stLim= 
		    String.concat ""( 
			      List.map
				  ( fun pos-> let letter= String.sub ioStr.stLim pos 1 in
					      transRevCube letter
				  ) 
				(enumF lenM) 
			      );
		  outLim=ioStr.outLim; (*output specifications*)
		  nowSt=ioStr.nowSt;
		  nxtSt=ioStr.nxtSt}
	)	
      aut
   ;;
(*divide stSp to sub-spaces with sBddL aut states:: make list intersaction*)
let divStSp2Aut  man (sBddL: vBDD_T list) (aut: ioType_T list) stSp =
  let num2BddM=num2BddMap man sBddL aut in
  let interPL= List.map (fun pair -> (fst pair, Bdd.dand stSp (snd pair)))  num2BddM in
  interPL;;

(*initialization of inp vars in stSp*)
let iniInpF (cirBdd:cirBdd_T) (aut: ioType_T list) stSp=
    let ss1= cleanSS cirBdd InpB stSp in (*delete all inps*)
    let autT= trans_AutF aut in
    let iniStL= List.filter (fun str-> str.nowSt=0) autT in (*choose initial state(s)- adges specifications*)
    let bddVL= List.concat (List.map (fun inV-> inV.vL) cirBdd.insVar) in
    let iniSSL= List.map (fun iniSt-> restrVarL bddVL iniSt.iLim ss1) iniStL in
    let orStSp= List.fold_left (Bdd.dor) (Bdd.dfalse cirBdd.manBdd) iniSSL in
    orStSp;;
(*initialization of mem cells in stSp*)
let iniMemF (cirBdd:cirBdd_T) (aut: ioType_T list) stSp=
    let ss1= cleanSS cirBdd MemB stSp in (*delete all mems*)
    let autT= trans_AutF aut in
    let iniStL= List.filter (*non-zero sepcific &&*)
	  (fun str-> (str.nowSt=0) && (str.stLim<>"") ) autT in (*choose initial state(s)- adges specifications*)
    let bddVL= List.concat (List.map (fun inV-> inV.vL) cirBdd.memVar) in
    let iniSSL= List.map (fun iniSt-> restrVarL bddVL iniSt.stLim ss1) iniStL in
    let orStSp= List.fold_left (Bdd.dor) (Bdd.dfalse cirBdd.manBdd) iniSSL in
    orStSp;;

(*annulation-initialization of aut state in stSp*)
let iniAutF (cirBdd:cirBdd_T)  stSp=
    let ss1= cleanSS cirBdd AutB stSp in (*delete all mems*)
    let bddVL= List.concat (List.map (fun inV-> inV.vL) cirBdd.statVar) in
    let zeroStSp= List.fold_left 
		    (fun ss var -> Bdd.dand ss (Bdd.dnot var)) 
		  ss1 bddVL in
    zeroStSp;;



(*update inputs based on already updated state of the state aut*)  
let updInpF (cirBdd:cirBdd_T) (aut: ioType_T list) stSp=
    let ss1= cleanSS cirBdd InpB stSp in (*delete all inpts for new restriction*)
    let autT= trans_AutF aut in (*translate edge notation to common sign notation*)
    let stStSepL= divStSp2Aut cirBdd.manBdd cirBdd.statVar aut ss1 in (*divide SS to aut states*)
    let stNumL= enumF (List.length stStSepL) in (*all state nums 0...n-1*)
    let pattLL = List.map  (*get list of inp limitations for each of states*)
		    (fun num-> ( num,
			List.map (fun str-> str.iLim) (*I need only inp limitations*)
			(List.filter (fun st-> st.nowSt=num) autT) (*state str corr2 this num*)
		      )
		    ) 
		stNumL	
    in 
    let bddVL= List.concat (List.map (fun inV-> inV.vL) cirBdd.insVar) in
    let oneStUp ssDiv pattL = (*restrict ssDiv in pattL ways - and join through OR*)
	  let restL = List.map 
			  (fun patt -> restrVarL bddVL patt ssDiv) 
		      pattL in
	  List.fold_left (Bdd.dor) (Bdd.dfalse cirBdd.manBdd) restL
    in
    let forEachStL = List.map2 
			(fun ssPD pattPL -> oneStUp (snd ssPD) (snd pattPL) ) 
		      stStSepL pattLL in
    List.fold_left (Bdd.dor) (Bdd.dfalse cirBdd.manBdd) forEachStL;;

  



