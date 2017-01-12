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

(*line_parts 	takes   *)	(* let line="G_62:	AND	(N_G10,N_G11),N_G32";; *)
(* 			 	returns*) (* val k : string list = ["G_79:"; "AND"; "(N_G93,N_G13),N_G94"] *)
	let line_parts line= 
			let line_split_tab= Str.regexp "\t" in
		Str.split line_split_tab line ;;
	
(*split_comma 	takes   *)	(* let line="eerf,wefw,erge, reg, f";; *)
(* 			 	returns*) (* val k : string list = ["eerf"; "wefw"; "erge"; " reg"; " f"] *)
	let split_comma line=
				let line_split_comma= Str.regexp "," in
		Str.split line_split_comma line;; 

(*gate_name - extracts the name:string of the gate from the input parameter line:string  *)
	let gate_name line = 
				let parts=line_parts line in
				let first_elem=
					try List.hd parts 
					with 
						Failure "hd"->""
					in
				let rexp_1st=Str.regexp "\\(.*\\):" in
			if (Str.string_match rexp_1st first_elem 0) then 
				Str.matched_group 1 first_elem
			else "ERR";;
(*gate_out - extracts the name:string of the gate output wire from the input parameter line:string  *)
	let gate_out line = 
				let parts=line_parts line in
				let inout_elem=try
						List.nth parts 2 
					with
						Failure "nth"->"ERR"
				in
				let inout_split=split_comma inout_elem in
		try
			List.nth inout_split ((List.length inout_split) -1)
		with 
			Failure "nth"->"ERR"
(*gate_ins - extracts the list of names:string for the gate inputs' names from the input parameter line:string  *)
	let gate_ins line = 
				let parts=line_parts line in
				let inout_elem=
					try
						List.nth parts 2 
					with
						Failure "nth"->"ERR"
				in
				let rexp_ins=Str.regexp "(\\(.+\\)).*" in
			try
				if (Str.string_match rexp_ins inout_elem 0) then 
				split_comma (Str.matched_group 1 inout_elem)
				else ["ERR"]
			with 
				Failure "nth"->["ERR"];;
(*gate_oper - extracts the gate operation:string from the input parameter line:string  *)		
	let gate_oper line = 
				let parts=line_parts line in
		try
			List.nth parts 1
		with
			Failure "nth"->"ERR"
(*rexp_cir- regular expression for the start of the circuit description *)
	let rexp_cir=Str.regexp "\\(.*\\)circuit\\(.*\\)";;

(*rexp_cir_end- regular expression for the end of the circuit description *)
	let rexp_cir_end=Str.regexp "\\(.*\\)endcircuit\\(.*\\)";;
	
(* if_cir: bool - checks if this line contains the word "circuit"= the start of the circuit description *)
	let if_cir line = (Str.string_match rexp_cir line 0);;
	
(* if_cir_end: bool - checks if this line contains the word "endcircuit"= the end of the circuit description *)
	let if_cir_end line = (Str.string_match rexp_cir_end line 0);;
	
(*circuit_discription- takes the filename and generates the string list only the lines that describe the circuit (including sub-circuits)  *)
	let circuit_discription filename=
			let all_lines = read_file filename in
			let include_skip line=
				if   ((gate_ins line) <> ["ERR"] ) (*and  (if_cir line) =  false*)  then
					line
				else ""
				in
		List.map  include_skip all_lines; 
	
(*gate_inf- record type which fully describes each gate used in the circuit*)
	type gate_inf=
		{	 
			name: string;
			op: string;
			ins: string list;
			out: string
		}
		
(*circuit_discription- takes the filename and generates the list gate_inf which fully describes the circuit (including sub-circuits)  *)
	(* TAIL-RECSIVE - manually checked *)
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
						Failure "hd"-> []  
						| Not_found -> List.tl line_list 
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
				Printf.sprintf "%s=%s and %s" g_info.out (List.nth(g_info.ins)0) (List.nth(g_info.ins)1)
			|"OR" -> 
				Printf.sprintf "%s=%s or %s" g_info.out (List.nth(g_info.ins)0) (List.nth(g_info.ins)1)
			|"NOR" -> 
				Printf.sprintf "%s=not(%s or %s)" g_info.out (List.nth(g_info.ins)0) (List.nth(g_info.ins)1)
			|"DFFX" -> 
				Printf.sprintf "%s=pre %s" g_info.out (List.nth(g_info.ins)0)
			|"NOT" -> 
				Printf.sprintf "%s=not %s" g_info.out (List.nth(g_info.ins)0)
			|_->""

(* delete_err-- function deletes the record from the input record list if it contains ERR in name field *)

  

	let pure_circuit_sub filename=(*FOR CHANGE- CHNAGE THIS- .bench parsing*)
		match (get_extention filename) with
		|"ckt"-> List.filter(fun record -> (record.name<>"ERR")) (circuit_structure filename)
		|"bench"-> fst (benITC_gRECL filename)
		| _ -> fst (benITC_gRECL filename) (*TODO:: change this by Risign the Failure*)
	    ;;			
(*str_to_eq_list-- takes the filename and generate the string list in Lustre-like HDL that describes the circuit (including sub-circuits)  *)
	let str_to_eq_list filename=
		let struc_list_full= pure_circuit_sub filename in
		let convert_to_eqs =
			List.map str_to_eq struc_list_full
		in 
		List.filter (fun x-> x<>"") convert_to_eqs;;
	
(*list_to_file-- function that takes string list as an input and write this list to the newly created file with the name filename*)	
	let list_to_file filename line_list =
				let chan_out = open_out filename in
				let line_to_file channel line=  
					output_string chan_out (line^"\n")
				in
				let write_list= line_to_file chan_out in
			List.iter write_list line_list;;
