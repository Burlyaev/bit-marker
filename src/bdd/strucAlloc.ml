open Cudd
open Lst
open CirG
open TimingGl
open IoType

type v_T = Cudd.Man.d Cudd.Bdd.t;;  (*Abstract type for BDDs *)
type man_T=Cudd.Man.d Cudd.Man.t ;; (*CUDD Manager*)

type bddVRole_T= InpB | OutB | MemB | GateB | None | AutB;;
type vBDD_T=(* the type of free varible represented through BDD *)
	{	 
	  vname: 	string; (* the name of the variable-gate*)
	  vPL: 		int list; (*position list of corresp vL*)
	  vL: 		v_T list;(* univesal encoding::2 mems for 4 bit encoding; *)
	  frVPL: int list;
	  frVL: v_T list;
	  vRole: bddVRole_T
	}
;;

type cirBdd_T=(* The BDD type how we save the circuit description after reading from the file  *)
  {
    fileC: 	string; (*file name*)
    manBdd:	man_T ; (*CUDD manager*)
 
    insVar: 	vBDD_T list ; (*base inputs*)
    memVar: 	vBDD_T list ; (*base mem cells*)
    outVar: 	vBDD_T list; (*base outputs cells*)	
    statVar: 	vBDD_T list; (*IO automata state bits*)
(*     gateVar: 	vBDD_T list ;  (*resolved gate vars relatevely base vars*) *) (*commented out for optimisation- i dont need intermediated BDD-gate functions*)
(*     nxtIVar: 	vBDD_T list;	 *)
    nxtMVar: 	vBDD_T list ; (*resolved nxt val mem cells relatevely base vars*)
    nxtOVar: 	vBDD_T list ;  (*resolved outs relatevely base vars*)
(*     nxtSVar: vBDD_T list (*resolved aut state vars*) *)
    };;

(********************* position allocation for variables*)
type bddPos_T=(* the type of free varible represented through BDD *)
	{	 
	  vnameP: 	string; (* the name of the variable-gate*)
	  pL: 		int list; (*position list of corresp vL*)
	  freePL: int list;
	}

type cirPos_T=(* keeps just the positions of the vars w/o allocation *)
  {
    fileP: 	string; (*file name*)
    multP:	int; 	(*encoding multiplicator*)

    insVP: 	bddPos_T list ; (*inputs *)
    memVP: 	bddPos_T list ;  (*free vars  *)
    statVP: 	bddPos_T list ;  (*IO automata bits  *)
    othVP: 	bddPos_T list ;  (*other vars - e.g. outputs *)
    };;


let posAlloc (cirStr:libGr) (ioAut :ioType_T list) (othVL:vertLab list)=
    let mult = meth2mult collP.meth in(*mult = bits per variable*)
    (* the seq of allocation: 
	    inputs with primes ; 
	    mem cells with primes*)
    let inpN= (List.length cirStr.inpVL) in
    let inpPosLL = List.map (fun inpNum -> 
			List.map (fun inpBitN-> 2*(inpNum*mult+ inpBitN)  )(enumF mult) ) 
		  (enumF inpN) 
    in
    let inp2posStructL= 
	  List.map2 
	    (fun vert posL -> 
			{
			  vnameP=vert.nameV;
			  pL= posL;
			  freePL = List.map (fun pos -> pos+1) posL
			  }
	    
	    ) 
	  cirStr.inpVL inpPosLL 
    in
    
    (*[TODO] possible optimization - there is no free var for inputs - so they don't used*)
    
    (*find all gate-mem.cells among all gates*)
     let memVL = List.filter (fun gateV -> gateV.opV= ItcBEN.memOpS) cirStr.gateVL in
     let memN = List.length memVL in
    (*mem vars position calc *)
    let memPosLL = List.map (fun memNum -> 
		      List.map (fun inpBitN-> 2*((inpN+memNum)*mult+ inpBitN)   )(enumF mult) ) 
		    (enumF memN) 
    in 
    
    (*free vars position calc are just +1*)
    let mem2posZip= 
	  List.map2 
	      (fun vert posL -> 
			{
			  vnameP=vert.nameV;
			  pL= posL;
			  freePL = List.map (fun pos -> pos+1) posL
			  }
	    
	    ) 
	  memVL memPosLL 
      in
    (*allocation of position of IO automaton*)
    let stateNum =  (maxStA ioAut) in (*+1 is not needed *)
    let toBits= int2binNum stateNum in
    let bitsNum= toBits.binLen in
    let stPosL= List.map (fun bitN -> {
					vnameP= "aut_"^(string_of_int bitN);
					pL= [2*(inpN+memN)*mult+ 2*bitN];
					freePL = [2*(inpN+memN)*mult+ 2*bitN+1]
				      }
		         )  
		    (enumF bitsNum) 
    in
    (*allocation of position of other needed vairbales - eg output signals*)
    let othNum= List.length othVL in
    let othVarPosLL =
	  List.map (fun othNum -> 
		      List.map (fun inpBitN-> 2*(inpN+memN)*mult +2*bitsNum+2*othNum*mult+ 2*inpBitN   )(enumF mult) ) 
		    (enumF othNum) 
    in
     let oth2posZip= 
	  List.map2 
	      (fun vert posL -> 
			{
			  vnameP=vert.nameV;
			  pL= posL;
			  freePL = List.map (fun pos -> pos+1) posL
			  }
	    
	    ) 
	  othVL othVarPosLL 
      in
    {
      fileP = cirStr.file;(*file name*)
      multP= mult; 	(*encoding multiplicator*)

      insVP= inp2posStructL ; (*inputs *)
      memVP =mem2posZip; (*memory*)
      statVP = stPosL; (*ioautomat variables*)
      othVP  = oth2posZip (*othe vars- for ex.:: outputs-> that I have to observe*)
    };;

    













