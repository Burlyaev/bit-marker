(* Parsing of the .banch benchmark from Italy ITX'99*)
(*read_file takes the name of the file to read it and returns the string list of the files lines*)
let read_file filename = 
		let lines = ref [] in
		let chan = open_in filename in
	try
	  while true; do
		lines := input_line chan :: !lines
	  done; []
	with End_of_file ->
	  close_in chan;
	  List.rev !lines ;; 
(*Record gate_inf- record type which fully describes each gate used in the circuit*)
type gate_inf=
	{	 
		name: string;
		op: string;
		ins: string list;
		out: string
	}

let recog_L line=
  let rexp_Iitc=Str.regexp "INPUT(\\(.*\\)).*" in
  let rexp_Oitc=Str.regexp "OUTPUT(\\(.*\\)).*" in
  let rexp_CIRitc=Str.regexp "\\(.*\\) = \\(.*\\)(\\(.*\\)).*" in
  let rexTL=[(rexp_Iitc,0);(rexp_Oitc,1);(rexp_CIRitc,2)] in

  let line_type=List.map (fun regexp-> ((Str.string_match (fst regexp) line 0),regexp)) rexTL in
  let corr_regext=List.filter (fun ass-> (fst ass)=true) line_type in
  let apply_rexp = List.map (fun regT-> snd regT) corr_regext in
  let extr_ins apply_rexp line=
      let fst_comp= ref "" in
      let snd_comp= ref "" in
      let thrd_comp=ref "" in
    fst_comp:=Str.matched_group 1 line;
    snd_comp:=Str.matched_group 2 line;
    thrd_comp:=Str.matched_group 3 line;
    let inps_dirt= Str.split (Str.regexp "[,]+") !thrd_comp in
    let inps=List.map (fun line->Str.global_replace (Str.regexp" ") "" line) inps_dirt in
    List.append [!fst_comp;!snd_comp] inps
  in
(*   try *)
   if ((List.length apply_rexp)>0) then 
      begin
	Str.string_match (fst (List.hd apply_rexp)) line 0;
	match snd (List.hd apply_rexp) with 
	  |0 -> (0,[Str.matched_group 1 line] )(*INPUT wire*)
	  |1 -> (1,[Str.matched_group 1 line] )(*OUTPUT wire*)
	  |2 -> (2, extr_ins apply_rexp line) (*de-composed equation*)
	  |_ -> (-1,["ERROR in matching of ITC'99"]);
      end
   else  (-2,[""]);;  
(*   with Failure _ -> (-2,[]) *)

let benITC_gRECL filename= 
(*   let chan = open_in filename in *)
  let lines=read_file filename in
  let pair_parced= List.map (fun line-> recog_L line) lines in
  let inpsPL = List.filter(fun pair-> (fst pair)=0) pair_parced in
  let inpL= List.concat (List.map(fun pair-> snd pair) inpsPL )in
  let outsPL= List.filter (fun pair-> (fst pair)=1) pair_parced in
  let outL= List.concat (List.map(fun pair-> snd pair) outsPL )in
  let cirPL=List.filter(fun pair-> (fst pair)=2) pair_parced in
  let cirL=(List.map(fun pair-> snd pair) cirPL )in
  let lstTOgateinf cir=
			{name=(List.nth  cir 0); 
			op= if ((List.nth cir 1)="DFF") then "DFFX" else (List.nth cir 1);
			ins=(List.tl (List.tl cir));
			out=(List.nth  cir 0)}

  in
  let gate_recL= List.map (lstTOgateinf)cirL in
(*   close_in chan; *)
 (gate_recL, (inpL,outL));;
(*  (inpL,outL);; *)


(* *********************** SUPPORTING - used for debugging *)
(* let filename= "b01_opt_r.bench";; *)
(* let line="# 2 outputs";; *)
(* let line="# 41 gates (1 and, 29 nand, 2 or, 8 not)\n";; *)
(* let line="INPUT(SI_30_)\n";; *)
(* let line="OUTPUT(OVERFLW_REG)" *)
(* let line="U72 = AND(nRESET_G, STATO_REG_1_, U76, STATO_REG_0_)";;

let line="nRESET_G, STATO_REG_1_, U76, STATO_REG_0_";;
*)