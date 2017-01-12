open Str
open Parser_itcBEN
(*Function "line_parts" 	takes   
    let line="G_62:	AND	(N_G10,N_G11),N_G32";; 
and returns
    val k : string list = ["G_79:"; "AND"; "(N_G93,N_G13),N_G94"] *)
	let line_parts line= 
			let line_split_tab= Str.regexp "\t" in
		Str.split line_split_tab line ;;	
(*Function "split_comma" takes
    let line="eerf,wefw,erge, reg, f";; 
and returns
    val k : string list = ["eerf"; "wefw"; "erge"; " reg"; " f"] *)
	let split_comma line=
				let line_split_comma= Str.regexp "," in
		Str.split line_split_comma line;; 
(* Function "gate_name" - extracts the name:string of the gate from the input parameter line:string  *)
	let gate_name line = 
				let parts=line_parts line in
				let first_elem=
					try List.hd parts 
					with 
						_->""
					in
				let rexp_1st=Str.regexp "\\(.*\\):" in
			if (Str.string_match rexp_1st first_elem 0) then 
				Str.matched_group 1 first_elem
			else "ERR";;
(*Function "gate_out" - extracts the name:string of the gate output wire from the input parameter line:string  *)
	let gate_out line = 
				let parts=line_parts line in
				let inout_elem=try
						List.nth parts 2 
					with
						_->"ERR"
				in
				let inout_split=split_comma inout_elem in
		try
			List.nth inout_split ((List.length inout_split) -1)
		with 
			_->"ERR"
(*Function "gate_ins" - extracts names:string list for the gate inputs' names from the input parameter line:string  *)
	let gate_ins line = 
				let parts=line_parts line in
				let inout_elem=
					try
						List.nth parts 2 
					with
						_->"ERR"
				in
				let rexp_ins=Str.regexp "(\\(.+\\)).*" in
			try
				if (Str.string_match rexp_ins inout_elem 0) then 
				split_comma (Str.matched_group 1 inout_elem)
				else ["ERR"]
			with 
				_ ->["ERR"];;
(* Function "gate_oper" - extracts the gate operation:string from the input parameter line:string  *)		
	let gate_oper line = 
				let parts=line_parts line in
		try
			List.nth parts 1
		with
			_ ->"ERR"
(*rexp_cir- regular expression for the start of the circuit description *)
	let rexp_cir=Str.regexp "\\(.*\\)circuit\\(.*\\)";;
(*rexp_cir_end- regular expression for the end of the circuit description *)
	let rexp_cir_end=Str.regexp "\\(.*\\)endcircuit\\(.*\\)";;
(*Function "if_cir":bool - checks if this line contains the word "circuit"= the start of the circuit description *)
	let if_cir line = (Str.string_match rexp_cir line 0);;
	
(*Function "if_cir_end": bool - checks if this line contains the word "endcircuit"= the end of the circuit description *)
	let if_cir_end line = (Str.string_match rexp_cir_end line 0);;
(*Function "circuit_discription"- takes the filename and generates the string list only the lines that describe the circuit (including sub-circuits)  *)
	let circuit_discription filename=
			let all_lines = read_file filename in
			let include_skip line=
				if   ((gate_ins line) <> ["ERR"] )  then
					line
				else ""
				in
		List.map  include_skip all_lines;; 
(*        *)
	  let circuit_discr_lst all_lines=
			let include_skip line=
				if   ((gate_ins line) <> ["ERR"] ) then
					line
				else ""
				in
		List.map  include_skip all_lines;; 
	

		
(*Function "circuit_structure"- takes the filename and generates the list of type gate_inf (records) which fully describes the circuit (including sub-circuits)  *)
	let circuit_structure filename=
			let line_list_full =circuit_discription filename in
			let rec line_to_str line_list = 
				let one_stuct line =
					{name=(gate_name line); 
					op=(gate_oper line);
					ins=(gate_ins line);
					out=(gate_out line)} 
				in
				let rest_list line_list =
					try
						 List.tl line_list
					with 
						_-> []  
(* 						| Not_found -> List.tl line_list  *)
				in
				if  (List.length(rest_list line_list) <>0) then
					line_to_str (rest_list line_list) @ [(one_stuct (List.hd line_list)) ]
				else [(one_stuct (List.hd line_list)) ]
			in
		line_to_str line_list_full ;;
(*  *)
      let circuit_str_lst line_input=
		    let line_list_full =circuit_discr_lst line_input in
		    let rec line_to_str line_list = 
			    let one_stuct line =
				    {name=(gate_name line); 
				    op=(gate_oper line);
				    ins=(gate_ins line);
				    out=(gate_out line)} 
			    in
			    let rest_list line_list =
				    try
					      List.tl line_list
				    with 
					    _-> []  
(* 					    | Not_found -> List.tl line_list  *)
			    in
			    if  (List.length(rest_list line_list) <>0) then
				    line_to_str (rest_list line_list) @ [(one_stuct (List.hd line_list)) ]
			    else [(one_stuct (List.hd line_list)) ]
		    in
	    line_to_str line_list_full ;;
		
(*str_to_eq-- take the record:gate_inf and returns the string in Lustre-like HDL to describe this gate*)
	let str_to_eq g_info =
		(* let operation gate_inf = *)
		match g_info.op with
			"AND" -> 
				Printf.sprintf "%s::\t %s=%s and %s"g_info.name g_info.out (List.nth(g_info.ins)0) (List.nth(g_info.ins)1)
			|"OR" -> 
				Printf.sprintf "%s::\t %s=%s or %s"g_info.name g_info.out (List.nth(g_info.ins)0) (List.nth(g_info.ins)1)
			|"NOR" -> 
				Printf.sprintf "%s::\t %s=not(%s or %s)"g_info.name g_info.out (List.nth(g_info.ins)0) (List.nth(g_info.ins)1)
			|"DFFX" -> 
				Printf.sprintf "%s::\t %s=pre %s"g_info.name g_info.out (List.nth(g_info.ins)0)
			|"NOT" -> 
				Printf.sprintf "%s::\t %s=not %s"g_info.name g_info.out (List.nth(g_info.ins)0)
			|_->""

(* delete_err-- function deletes the record from the input record list if it contains ERR in name field *)
let get_extention filename =
  let name_separated= Str.split (Str.regexp "[.]+") filename in
  List.hd (List.rev name_separated);;

	let pure_circuit_sub filename=(*FOR CHANGE- CHNAGE THIS- .bench parsing*)
		match (get_extention filename) with
		|"ckt"-> List.filter(fun record -> (record.name<>"ERR")) (circuit_structure filename)
		|"bench"-> fst (benITC_gRECL filename)
		| _ -> fst (benITC_gRECL filename) (*TODO:: change this by Risign the Failure*)
	    ;;
(*	let pure_circuit_sub filename=(*FOR CHANGE- CHNAGE THIS- .bench parsing*)
		let gate_inf_list= circuit_structure filename in
		List.filter  (fun record -> (record.name<>"ERR")) gate_inf_list ;;*)
(*  *)
	let pure_circuit_sub_lst inp_list=
		let gate_inf_list= circuit_str_lst inp_list in
		List.filter  (fun record -> (record.name<>"ERR")) gate_inf_list ;;

  let no_internal_dff gate_rec_list= 
    List.filter (fun record -> 
	    (
	      try 
		String.sub (record.name) 0 2
	      with  _ -> ""
		  )<>"ff") 
    gate_rec_list;;

(*str_to_eq_list-- takes the filename and generate the string list in Lustre-like HDL that describes the circuit (including sub-circuits)  *)
	let str_to_eq_list filename=
		let struc_list_full= pure_circuit_sub filename in
		let convert_to_eqs =
			List.map str_to_eq struc_list_full
		in 
		List.filter (fun x-> x<>"") convert_to_eqs;;
	
(*list_to_file-- function that tkaes string list as an input and write this list to the newly created file with the name filename*)	
	let list_to_file filename line_list =
				let chan_out = open_out filename in
				let line_to_file channel line=  
					output_string chan_out (line^"\n")
				in
				let write_list= line_to_file chan_out in
			List.iter write_list line_list;
			close_out chan_out;;
(*newly-added funciton  to add some information to the INFORMATION file about particular benchmark *)
      	let lstAddFile wORa filename line_list_ref =
				let chan_out = 
				  match wORa with 
				  | "a" ->    open_out_gen [Open_wronly; Open_append; Open_creat] 0o600 filename 
				  | _ 	->    open_out filename 
				in
				let line_to_file channel line=  
					output_string channel (line^"\t")
				in
				let write_list= line_to_file chan_out in
			Array.iter write_list !line_list_ref;
			output_string chan_out ("\n");
			close_out chan_out;
			line_list_ref:=[||];;

(* extraction of primary input/output lists from the files*)

      let extract_smth filename what_extract=
	  let all_lines = read_file filename in
	  let str_for_rexp= "\\(.*\\)"^what_extract^"\\(.*\\)" in 
	  let rexp_ins = Str.regexp str_for_rexp in
	List.filter (fun line -> Str.string_match rexp_ins  line 0) all_lines ;;
(*  *)












