open Parser_itcBEN
open Graph
open Graph_build
open CombTOseq2
open Ckt_to_l2hdl
(*inputs/outputs listing from file of a netlist*)
    let inputs_for_n_circuit 
			filename (*netlist file*)
			      n 
				  what_extract (*inputs or outputs*)
					    =
	  let ckt_sign_extr filename n what_extract=
	    let input_lines = List.nth (extract_smth filename what_extract) n in (*we just thake the fst line that contains what to extract*)
	    let string_for_rexp ="\t"^what_extract^"\t\\(.+\\)"  in
	    let rexp_ins_string=Str.regexp string_for_rexp in
	    try
	    if (Str.string_match rexp_ins_string input_lines 0) then (*?? a bit redundant with extract_smth function ??*) 
	      split_comma (Str.matched_group 1 input_lines)
	      else ["ERR"]
	    with 
	      _ ->["ERR"]
	   in
	   let bench_sgin_extr filename n what_extract =
	      let res=snd (benITC_gRECL filename) in
	      match what_extract with 
		|"inputs" ->fst res
		|"outputs"-> snd res
		| _ ->snd res(*TODO:: change this by Risign the Failure*)
	   in

	   match (get_extention filename) with 
	    |"ckt"-> ckt_sign_extr  filename n what_extract
	    |"bench"-> bench_sgin_extr filename n what_extract
	    | _ -> ckt_sign_extr  filename n what_extract (*TODO:: change this by Risign the Failure*)
	   ;;
