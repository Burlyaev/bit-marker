(*********	Program Starts HERE!	******)
print_string "Program Started!\n";;
(*  *)
let start_file =0;;
let file_to_process=0;;(*max 29- there are 32 circuits in ISCAS'89*)
(* TRANSLATION to L-HDL  *)
(*       list_to_file filename_out (str_to_eq_list filename);; (*Saving L-HDL to filename_out*) *)
      (*the Lustre-like program is ready after this point*)
(* GRAPH GENERATION*)
(*       let gate_rec_list=no_internal_dff (pure_circuit_sub filename);; (*generation of gate_rec list, wo sub-circuits*) *)
(*       let graph= build_graph gate_rec_list;; (*"graph" represents the seq circuit with combin circuit*) *)
(*       let graphSstruct= build_graph_MEM filename;;(*"graphS" represents the structure with the seq circuit ONLY*) *)
(* DRAWING THE GRAPH TO THE FILE *)
      (*export_graph graph filename_graph;; (*writing SEQ-comb graph .dot representation*) *)
      (*export_graph (fst graphSstruct) filename_graph_MEM;; (*writing SEQ-comb graph .dot representation*) *)

(*VOTER INSERTION ALGORITHMs*)
let infA=ref [||] in
let add_to_inf new_inf = 
	  infA:=Array.append !infA [|new_inf|] in
let its x = string_of_int x in

for i=start_file to ((start_file+file_to_process)-1) do
   infA:=[||];
   let filename=all_cir_files.(i) in
   print_string ("\n\nItteration:: "^(string_of_int i)^" ::\t"^(all_cir_names.(i))^"\n");
   
   (*GRAPH EXPORTING*)
  
    let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
    let graph= build_graph gate_rec_list in (*"graph" represents the seq circuit with combin circuit*)
    let graphSstruct= build_graph_MEM filename in(*"graphS" represents the structure with the seq circuit ONLY*)
    export_graph graph all_grALL_files.(i);
    export_graph (fst graphSstruct) all_grSEQ_files.(i)

  (*INF GATHERING abotu benchmark*)
(*   let minVerSet=  minVS curDir filename in(*find Minimum Feedback Vertex Set*)  *)
(*   let num_MVS=  snd minVerSet in *)
(*    let lAcycPath= long_acyc_path filename in *)
(*    let len_APath= snd lAcycPath in *)
(*    let hop_step= max 1 (len_APath/10) in *)
(*      let len_APath= 10 in
      let hop_step= 1 in
      add_to_inf "len_acyc_path";
      add_to_inf (its len_APath);
      add_to_inf "hops_step";
      add_to_inf (its hop_step);*)
(*    let lNonLoopPath = find_cycFREE_path filename in (*not very useful*) *)
(*       add_to_inf "MVS"; *)
(*    add_to_inf num_MVS; *)
(*    add_to_inf lNonLoopPath; (*not very useful*) *)

(*       lstAddFile "w" all_INF_files.(i) infA ; *)

 (* VOTER INSERTING *)
   
(*   let file_list_s= [all_cir_files.(i)] in *)
  
(*   through_files_UNI find_GoptOUT hop_step len_APath file_list_s (vo_file "opt" i) (time_file "opt" i);(*READY*) *)

(*   through_files_UNI find_Gfst hop_step len_APath file_list_s (vo_file "Gfst" i) (time_file "Gfst" i); (*READY. Very non-optimal*)  *)

(*   through_files_UNI find_Mvo hop_step len_APath file_list_s (vo_file "Mvo" i) (time_file "Mvo" i); (*READY*) *)

(*   through_files_UNI p_print_Pps hop_step len_APath file_list_s (vo_file "Pps" i) (time_file "Pps" i); (*SEMI_READy, W1*) *)

(*   through_files_UNI find_GPMS hop_step len_APath file_list_s (vo_file "GPMS" i) (time_file "GPMS" i); (*SEMI_READy, W3*) *)

(*   through_files_UNI find_STRG  hop_step len_APath file_list_s (vo_file "STRG" i) (time_file "STRG" i); (*READY*)  *)

(*   through_files_UNI find_MIXed  hop_step len_APath file_list_s  (vo_file "Mixed" i) (time_file "Mixed" i);; (*READY *) *)

done;
print_string "Program Finished!\n";;
