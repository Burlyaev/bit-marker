open Cudd
open TimingGl
open StrucAlloc
open VarResolv
open CirG
open BddShrink
open MethChoice
open IoType
open IoAutom
open InpAutom
(* cir2bdd function converts the circuit in the filename to BDD represintaiton f type cirBdd_T*)
(*global param.struct. contains in collP*)
let cir2bdd (votL:string list) (filename:string) (ioAut :ioType_T list) 
=	
infO#tim "cir2bdd: Start";
    let cirStr = itc2libG filename in (*cir disctiption from netlist*)
infO#tim "cir2bdd: cirStr";   
    let posStr= posAlloc cirStr ioAut cirStr.outVL in (*positions of all my vars*)
infO#tim "cir2bdd: posStr"; 

(*
    let insL= cirStr.inpVL in  
    let outsL= cirStr.outVL in

    let memL= List.filter 
		(fun var->  ((str2OpTvar.opV) = DffF)) 
	    cirStr.gateVL in

    let combL= List.filter 
		(fun grec-> not (beDFF grec.op))
	     cirStr.gateVL in
*)

infO#tim  "cir2bdd: file read Done";
    let man = Man.make_d ~numVars:(0) ~maxMemory:(1 lsl 30) () in (*make_d*)
    let bddAlloc num = Bdd.ithvar man num  in (*function to return num bdd var*)
  (* #1 - instantiate vars for  inputs and for MEM cells - EXE ONCE*)
infO#tim  "cir2bdd: Manager Created";
    let insBddL= List.map 
		  (fun  posStr -> 
			    {
			      vname = 	posStr.vnameP;
			      vPL =	posStr.pL; 
			      vL  =  	List.map (fun pos -> bddAlloc pos) posStr.pL;		
			      frVPL = 	posStr.freePL;
			      frVL  = 	List.map (fun pos -> bddAlloc pos) posStr.freePL;
			      vRole = 	InpB
			    }
		  )
	       posStr.insVP
    in
infO#tim  "cir2bdd: InpV allocated";
  (*seq num of Bdd for mem*)
    let memBddL= 
	    List.map 
		  (fun  posStr -> 
			    {
			      vname = 	posStr.vnameP;
			      vPL = 	posStr.pL; 
			      vL  =  	List.map (fun pos -> bddAlloc pos) posStr.pL;		
			      frVPL = 	posStr.freePL;
			      frVL  = 	List.map (fun pos -> bddAlloc pos) posStr.freePL;
			      vRole = 	MemB
			    }
		   )
	        posStr.memVP
    in
infO#tim  "cir2bdd: MemV allocated";
    (*we first introduce voters after mem cells and resolve the nxt state relatevely to voters*)
    (*  let memVotBddL= 
	  List.map 
	  (fun bdd -> match (List.exists (fun vot -> vot=bdd.vname) votL) with  
		      | true ->  logOps.votOp bdd(*we intro vot after this mem cell*)
		      | false -> bdd (*NO vot after this mem cell*)
	  ) 
	  memBddL in*)
    (*[DECISITION] before memory cells - but possible after-see above     *)
    let knownL= insBddL @ memBddL in(*the initial knownListR of BDD variables; they are free variables: input and mem veriables (!!! with voters)*)
  
    (*  restrict Reordering if it will happen *)
    fixReordM1 man posStr; (*fixation of (v,v') pairs during reordering*)
    (* resolve combinatarial part relatevey to inps and mem cells*)

    let knMCLR= resolvVars man cirStr knownL  in(* #2 - for the NEXT iteration we have to express new VARs throught these variables *) 
infO#tim  "cir2bdd: Comb Cir Resolved";
     let memL = List.filter (fun gateV -> str2OpT(gateV.opV)= DffF) cirStr.gateVL in
    let knMenNxtLR= resolvVarNxt man knMCLR memL  in(* #4- now when all combin part are resolved, we go to the next cycles and resolve again sequential part *) (* MEMs- new seq cells *)
infO#tim  "cir2bdd: SeqNxtMem Resolved";
  (* classify by category     *)
    let nxtOutBddL=   List.filter (fun vBdd-> vBdd.vRole=OutB) knMCLR in
(*     let gateBddL=  List.filter (fun vBdd-> vBdd.vRole=GateB) knMCLR in *)
infO#tim  "cir2bdd: Finished";
infO#tim  "cir2bdd: IO aut. start";
    let outBddL= List.map 
		  (fun  posStr -> 
			    {
			      vname = 	posStr.vnameP;
			      vPL =	posStr.pL; 
			      vL  =  	List.map (fun pos -> bddAlloc pos) posStr.pL;		
			      frVPL = 	posStr.freePL;
			      frVL  = 	List.map (fun pos -> bddAlloc pos) posStr.freePL;
			      vRole = 	OutB
			    }
		  )
	       posStr.othVP
    in
    let statBddL= List.map 
		  (fun  posStr -> 
			    {
			      vname = 	posStr.vnameP;
			      vPL =	posStr.pL; 
			      vL  =  	List.map (fun pos -> bddAlloc pos) posStr.pL;		
			      frVPL = 	posStr.freePL;
			      frVL  = 	List.map (fun pos -> bddAlloc pos) posStr.freePL;
			      vRole = 	AutB
			    }
		  )
	       posStr.statVP
    in
   (* let nxtStatVarL= resAutoSF man statBddL ioAut in*)
  {
    fileC=filename; (*netlist file name*)
    manBdd=man; (*BDD manager*)

    insVar=insBddL;  (*base inputs*)
    memVar=memBddL; (*base mem cells*)
    outVar=outBddL; (*base outputs cells*)	
    statVar=statBddL; (*IO automata state bits*)

(*     gateVar=gateBddL; (*resolved gate vars relatevely base vars*) *)
(*     nxtIVar=nxtInpVarL;(*resolved input vars rel. base state vars*) *)
    nxtMVar=knMenNxtLR; (*resolved nxt val mem cells relatevely base vars*)
    nxtOVar=nxtOutBddL; (*resolved outs relatevely base vars*)
(*     nxtSVar=nxtStatVarL (*resolved automaton state variables*) *)
  };;

(* --EOF-- *)




