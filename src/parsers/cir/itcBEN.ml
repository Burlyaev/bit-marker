open File


(* Parsing of the .banch benchmark from Italy ITC'99*)
let memOpS="DFFX";;

	
(*Record gate_inf- record type which fully describes each gate used in the circuit*)
type gate_inf=
	{	 
		name: string;
		op: string;
		ins: string list;
		out: string
	}

let rexp_Iitc=Str.regexp "INPUT(\\(.*\\)).*" ;; (*reg expresion - inps*)
let rexp_Oitc=Str.regexp "OUTPUT(\\(.*\\)).*" ;; (*reg expresion -outps*)
let rexp_CIRitc=Str.regexp "\\(.*\\) = \\(.*\\)(\\(.*\\)).*" ;;(*reg expresion- gate*)

(*recognises the line of netlist- returns the type of line and the structural information about the gate: if it's an input/output/gate*)
exception ParcLine of int;;
let recog_L line= (*line is a line of a netlist file*)
  
  let rexTL=[(rexp_Iitc,0);(rexp_Oitc,1);(rexp_CIRitc,2)] in (*list of regexps; enumerated 0,1,2*)

  let line_type=List.map  (*each pattern is tested on a line*)
		    (fun regexp-> ((Str.string_match (fst regexp) line 0),regexp)) 
		 rexTL 
  in
  let corr_regext=List.filter (fun ass-> (fst ass)=true) line_type in (*filter unsuccessful recognitions*)
  let apply_rexp = List.map (fun regT-> snd regT) corr_regext in (*keep only the result of a recognition*)

  let gateParse apply_rexp line= (*extraction-funtion of gate inputs- there can be several*)
    let fst_comp =Str.matched_group 1 line in (*fst input- gate output*)
    let snd_comp =Str.matched_group 2 line in (*snd input- gate operation*)
    let thrd_comp=Str.matched_group 3 line in (*snd input- gate inputs in 1 stream*)
    let inps_dirt= Str.split (Str.regexp "[,]+") thrd_comp in
    let inps=List.map (fun line->Str.global_replace (Str.regexp" ") "" line) inps_dirt in (*clean spaces*)
    List.append [fst_comp;snd_comp] inps
  in

  match (List.length corr_regext) with 
      | 0 -> (-2,[]) ; (*expected trash - not all lines mean smth*)
      |	1  ->  (begin 
		  ignore (Str.string_match (fst (List.hd apply_rexp)) line 0);
		  match (snd (List.hd apply_rexp)) with 
		  |0 -> (0,[Str.matched_group 1 line] )(*INPUT wire*)
		  |1 -> (1,[Str.matched_group 1 line] )(*OUTPUT wire*)
		  |2 -> (2, gateParse apply_rexp line) (*de-composed equation for gates*)
		  | _ -> raise ( ParcLine (snd (List.hd apply_rexp))) (*impossible case*)
		end
		)	      
      | _ -> raise ( ParcLine (List.length corr_regext))(*("Parcing:: more than one match in match")*)  
  ;;

(*
[DEBUG]
  let a= read_file filename;;
  let line= (List.nth a 1);;
  recog_L line;; 
*)
type cir_inf=
      {	
	file  : string;  (*name of netlist file*)
	inpL : string list; (*names of inps*)
	outL : string list; (*names of outs*)
 	gateL: gate_inf list; (* all gates describtion*)
	}

let benITC_gRECL filename= 
  let lines=read_file filename in (*lines from file*)
  let pair_parced= List.map (fun line-> recog_L line) lines in (*parced file*)
  let inpsPL = List.filter(fun pair-> (fst pair)=0) pair_parced in (*inputs pairs list*)
  let inpL= List.concat (List.map(fun pair-> snd pair) inpsPL) in (*inputs list*)
  let outsPL= List.filter (fun pair-> (fst pair)=1) pair_parced in 
  let outL= List.concat (List.map(fun pair-> snd pair) outsPL )in  (*outputs list*)
  let cirPL=List.filter(fun pair-> (fst pair)=2) pair_parced in 
  let cirL=(List.map(fun pair-> snd pair) cirPL )in (*gates list*)
  let lstTOgateinf gate=
			{name=(List.nth  gate 0); (*gate name*) 
			op= (match (List.nth gate 1) with
			      |"DFF" -> memOpS (*we just rename DFF-> DFFX*)
			      |_ -> (List.nth gate 1));
			ins=(List.tl (List.tl gate));
			out=(List.nth  gate 0)}
  in
  let gate_recL= List.map (lstTOgateinf) cirL in
  {
    file= filename; 
    inpL =inpL;
    outL= outL;
    gateL= gate_recL;
  }
;;



(* *********************** SUPPORTING - used for debugging *)
(* let filename= "b01_opt_r.bench";; *)
(* let line="# 2 outputs";; *)
(* let line="# 41 gates (1 and, 29 nand, 2 or, 8 not)\n";; *)
(* let line="INPUT(SI_30_)\n";; *)
(* let line="OUTPUT(OVERFLW_REG)" *)
(* let line="U72 = AND(nRESET_G, STATO_REG_1_, U76, STATO_REG_0_)";;

let line="nRESET_G, STATO_REG_1_, U76, STATO_REG_0_";;
*)