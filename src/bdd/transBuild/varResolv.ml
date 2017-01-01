open CirG
open MethChoice
open TimingGl
open StrucAlloc
open BddShrink
(* nToResolve -- function indicates if there are variables that are still not resolved *)
(*[OPT] quite heavy - optimize*)
let nToResolve (cirStr:libGr) (knownL: vBDD_T list)  =
    let gateAndOut= cirStr.gateVL @ cirStr.outVL in  (*gates and outs*)
     
    (* condition: not in the list 
			    of resolved parameters*)
    let iNotRes varTest= 
	List.for_all 
	  (fun var-> var.vname <> varTest.nameV ) 
	knownL
    in
    
    (*condition:: all inputs for the candiate are resolved*)
    let argRes varTest= 
      List.for_all 
	  ( fun inName-> 
		List.exists (fun known -> known.vname=inName)  knownL
	  ) 
      varTest.insV
    in
    try
      let candidate= 
	List.find 
	(fun varTest-> 
	    (iNotRes varTest) && (*var not yet resolved*)
	    (argRes varTest) 	(*but all its inputs are resolved*)
	) 
	gateAndOut 
      in
      [candidate]

    with 
      Not_found ->[];; (*no such var exsists= "all are resolved"*)
    

(* ------------------------------------------------------------------- *)


(* resolveF -- takes the known list, combinational list, and name of variable to resolve;
returns the BDD resolved though known BDDs of input signals*)
let resolveF (knownL: vBDD_T list)  (varN:vertLab) =
  let insL= varN.insV in
  let insBddL= List.map 
		  (fun insName -> 
			List.find 
			    (fun vBdd-> vBdd.vname = insName  )
			knownL ) 
		insL
  in
  begin
infO#tim ("[resolveF]"^(let a= List.hd insBddL in a.vname));
  logFun varN insBddL 
  end;;
  
(*resolvVars -- function that resolved all combination signals based on free variables and returns the resolved list*)
let resolvVars man (cirStr:libGr) (knownL:vBDD_T list)=
  let unknownL= (cirStr.outVL) @ (*resolve outputs*)
		  (List.filter (*and all comb vars- w/o mem. cells*)
			  (fun gate-> (str2OpT gate.opV)<>DffF)
		  cirStr.gateVL) in
  let rec iterResolv  knownR_Old unknown_Old iterat= (*iterat needed to reorder BDD when it's big*)
    let condStop= nToResolve cirStr knownR_Old in (*more non-resolved?*)
    
    reorderF man iterat; (*reorder if needed&alloved*)

infO#tim ("BDD build:resolvVars::\t"^(string_of_int(List.length condStop)));
    
    if (List.length condStop >0) then (*more non-resolved?*)
	let vert2resolv =List.hd condStop in (*choose non-resolved*)
infO#tim (vert2resolv.nameV);
	let newKnownBdd = resolveF knownR_Old vert2resolv in
infO#tim ("resolved::"^newKnownBdd.vname); 
	let knownR_New=  knownR_Old @ [newKnownBdd] in

	let unknown_New= List.filter 
		  (fun gInf-> gInf.nameV<>vert2resolv.nameV) unknown_Old 
	in
	iterResolv knownR_New unknown_New (iterat+1)(* recurive call *)
    else 
    begin
infO#tim ("resolvVars- done");
      knownR_Old(*return the resolved list*)
    end
  in
  iterResolv knownL unknownL 0;;


(* resolvVarNxt -- func. resolves the mem vars relatevely to comb vars*)
let resolvVarNxt man  (knListR: vBDD_T list) (mNxtL:vertLab list)=
  let rec resMemNxt  memNxtL accum=
      infO#tim  ("BDD build:resolvVarNxt::\t"^(string_of_int 
						  (List.length memNxtL)));
    if ((List.length memNxtL)>0) then 
      let memOne= List.hd memNxtL in (*we take the 1st non-resolved*)
      let memResolved= resolveF knListR memOne  in
      resMemNxt  (List.tl memNxtL) (accum @ [memResolved])
    else
      accum
  in
 resMemNxt mNxtL [];;





