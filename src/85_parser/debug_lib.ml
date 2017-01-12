#use for everything in the beginning after we found all gate records
let pr_inps_list= inputs_for_n_circuit filename 1 "inputs";;
let pr_outs_list= inputs_for_n_circuit filename 1 "outputs";;
let all_v= find_all_vertex graph;;


#use for optimization algorithms
let s1= VS.empty;;(* TEST *)
let s2= VS.empty;;(* TEST *)
let s3= VS.empty;;(* TEST *)
let s4= VS.empty;;(* TEST *)

#use "optim_dist.ml"

let vert_names=find_out_vert_names gate_rec_list pr_outs_list;;
let all_v= find_all_vertex graph;;

let out_vers=find_out_vert_list all_v vert_names;; 
let ini_set = v_list_to_set out_vers;;
let v_set=ini_set;;(* TEST *)


let s2= successor_set graph ini_set;; (* TEST *)
let v=List.nth (VS.elements s2) 0;;(* TEST *)

let s3=hop_k_times 1 graph ini_set vo_set;;(* TEST *)
G.V.label (List.nth (VS.elements s3) 0);;(* TEST *)

let nvl all_v=
  List.map (fun el-> G.V.label el) all_v;;


names_to_be_protected k graph v_set vo_set;;(* TEST *)


#use "optim_path.ml";;
let k=2;;(* TEST *)
let vo_set=VS.empty;;(* TEST *)


let ini_path_type_list= List.map (fun v->{p=[v]; code =2}) (VS.elements ini_set);;(* TEST *)
let path={p=[(List.nth all_v 0)]; code =2};;(* TEST *)
let a=[{p=[(List.nth all_v 0)]; code =2}];;(* TEST *)


let path_type_list=ini_path_type_list;;
let a =hop_1_time_paths_list_from_pathes graph path_type_list vo_set;;(* TEST *)
let a =hop_1_time_paths_list_from_pathes graph a vo_set;;(* TEST *)

let (vo_set,b)= loop_elim vo_set path_type_list;;


(*let rec hop_k_time_paths_list_from_pathes k graph path_type_list ini_path_type_list vo_set=*)

let a=ini_path_type_list;;(* TEST *)
let (b,c)= hop_k_time_paths_list_from_pathes 1 graph a a vo_set ;;(* TEST *)

(* let rec voter_intro k graph path_type_list vo_set= *)


(*COMMENT::new_vo_set minus vo_set- correspond to all voters been intoduced*)
(* let a0=[{p=[(List.nth all_v 0)]; code =2}];;(* TEST *) *)
let a0=ini_path_type_list;;
let vo_set=VS.empty;;(* TEST *)
let d= voter_intro 1 graph a0 vo_set;;

#use "optim_pascal.ml"

let print_path path = 
  let ver_lst= path.p in
    List.map (fun v-> G.V.label v)ver_lst;;

let print_all_p all_p= 
  List.map print_path all_p ;;

List.map (fun v-> G.V.label v) my_res;;

#use "optim_pas_ms.ml";;

List.map (fun a-> G.V.label (fst a)) !ass_v_tab;;  
List.map (fun a-> G.V.label (fst a)) !ass_v_Ftab;;

let get_pred all_v ass_v_tab k =
  let ver=List.nth all_v k in
  let assoc_tab= List.assoc ver ass_v_tab in
  let pred_arr= assoc_tab.sv_s in
  Array.iter (fun pred-> print_string ((G.V.label pred)^"\t")) pred_arr;;


get_pred all_v !ass_v_tab 0;;

let p_t ass_v_tab=
  let names_pred_lst pred_arr =
  Array.iter (fun pred-> print_string ((G.V.label pred)^"\t")) pred_arr
  in
  let val_counters assoc_tab =
    let counters = assoc_tab.indx in
    Array.iter (fun pred-> print_string ((string_of_int pred)^"\t")) counters
  in
  let func1 ass_v=
    let ver= fst ass_v in
    let ver_name= G.V.label ver in
    let assoc_tab= snd ass_v in
    let pred_arr= assoc_tab.sv_s in
  print_string (ver_name^"::");
  names_pred_lst pred_arr;
  val_counters assoc_tab;
  print_string "\tEND\n "
  in
List.iter func1 ass_v_tab;;

p_t !ass_v_tab;;

let p_Ft ass_v_Ftab=
  let names_succ_lst succ_arr =
  Array.iter (fun succ-> print_string ((G.V.label succ)^"\t")) succ_arr
  in
  let val_counters assoc_Ftab =
    let counters = assoc_Ftab.ctr in
    Array.iter (fun succ-> print_string ((string_of_int succ)^"\t")) counters
  in
  let func1 ass_v=
    let ver= fst ass_v in
    let ver_name= G.V.label ver in
    let assoc_Ftab= snd ass_v in
    let succ_arr= assoc_Ftab.sv_Fs in
  print_string (ver_name^"::");
  names_succ_lst succ_arr;
  val_counters assoc_Ftab;
  print_string "\tEND\n "
  in
List.iter func1 ass_v_Ftab;;


p_Ft !ass_v_Ftab;;

let p_p points_v_tab =
  let func1 ass_v=
      let ver= fst ass_v in
      let ver_name= G.V.label ver in
    print_string (ver_name^"\t::\t"^(string_of_int (snd ass_v)));
    print_string "\tEND\n "
  in
List.iter func1 points_v_tab;;

p_p !points_v_tab;;

#END use "optim_pas_ms.ml";;



List.iter (fun path ->
  Printf.printf "%s\n"
  (String.join " " (List.map (fun e -> string_of_int (G1.V.label e)) path))
) ll





List.iter (fun path ->
  Printf.printf "%s\n"
  (String.join " " (List.map (fun e -> (G.V.label e)) path))
) (conv_cyc_G1_to_G all_Gv_arr all_G1v_arr all_v ll)




(* FOR DELETE *)


(* YOU CAN DELETE BELOW EVERYTHING _ FOR CLEANING since UNI function replaces all below *)
(*  
based on optim_dist.ml
*)
let rec str_build_Gfst step_k k filename=
  let startF= ref 0.0 in
  let finishF= ref 0.0 in
  let res_temp = ref[] in
  let count = ref 0 in
  let vo_num_str = ref "" in
  let times_str = ref "" in

  startF:= Unix.gettimeofday() ;
  res_temp:=[find_Gfst filename 1];
  finishF:= Unix.gettimeofday() ;
    let output_str= string_of_int (snd(List.hd !res_temp)) in
  vo_num_str:=((!vo_num_str)^output_str^"\t");
      let timeDIFF_str= Printf.sprintf "%f" (!finishF -. !startF ) in
  times_str:=((!times_str)^timeDIFF_str^"\t");
  count:=step_k;
  while !count< k do
    startF:= Unix.gettimeofday() ;
    res_temp:=[find_Gfst filename !count];
    finishF:= Unix.gettimeofday() ;
      let output_str= string_of_int (snd(List.hd !res_temp)) in
    vo_num_str:=((!vo_num_str)^output_str^"\t");
      let timeDIFF_str= Printf.sprintf "%f" (!finishF -. !startF ) in
    times_str:=((!times_str)^timeDIFF_str^"\t");
    count:=!count+ step_k
  done;
[!vo_num_str; !times_str];;

let begin_end_Gfst step_k k filename =
    let result= str_build_Gfst step_k k filename in
    [ (* filename^"::\t" ^*)(List.nth result 0)^"\n";(List.nth result 1)^"\n"];;

let through_files_Gfst step_k k file_list_s filename_stat filename_time =
  let lst_str=List.map (begin_end_Gfst step_k k) file_list_s in
  let vo_num = List.map (fun el-> List.nth el 0) lst_str in
  let time_num = List.map (fun el-> List.nth el 1) lst_str in
  list_to_file filename_stat vo_num;
  list_to_file filename_time time_num;;
(* 	through_files_Gfst 	5 130 file_list_s filename_stat filename_time;; *)

(*  *)
(*  
based on optimal_sol.ml
*)
let rec str_build_OPT step_k k filename=
  let output= find_GoptOUT filename k  in
  let vo_num= snd output in
  let vo_num_str= string_of_int vo_num in
    if (k>1) then 
      let next_str= str_build_OPT step_k (k-step_k) filename in
      next_str^"\t"^vo_num_str
    else 
      vo_num_str;;

let begin_end_OPT step_k k filename =
    let result= str_build_OPT step_k k filename in
     (* filename^"::\t" ^*)result^"\n";;

let through_files_OPT step_k k file_list_s filename_stat =
  let lst_str=List.map (begin_end_OPT step_k k) file_list_s in
  list_to_file filename_stat lst_str;;
(* 	through_files_OPT  	1 10 file_list_s filename_stat;;  *)


(*  *)
(*  
based on dist_making.ml
*)
let rec str_build_M step_k k filename=
  let output= find_Mvo filename k  in
  let vo_num= snd output in
  let vo_num_str= string_of_int vo_num in
    if (k>1) then 
      let next_str= str_build_M step_k (k-step_k) filename in
      next_str^"\t"^vo_num_str
    else 
      vo_num_str;;

let begin_end_M step_k k filename =
    let result= str_build_M step_k k filename in
     (* filename^"::\t" ^*)result^"\n";;

let through_files_M step_k k file_list_s filename_stat =
  let lst_str=List.map (begin_end_M step_k k) file_list_s in
  list_to_file filename_stat lst_str;;
(* 	through_files_M  	1 10 file_list_s filename_stat;;   *)


(*  *)
(*  
based on optim_pascal.ml
*)
let rec str_build_P step_k k filename=
  let output= p_print_Pps filename k  in
  let vo_num= snd output in
  let vo_num_str= string_of_int vo_num in
    if (k>1) then 
      let next_str= str_build_P step_k (k-step_k) filename in
      next_str^"\t"^vo_num_str
    else 
      vo_num_str;;

let begin_end_P step_k k filename =
    let result= str_build_P step_k k filename in
     (* filename^"::\t" ^*)result^"\n";;

let through_files_P step_k k file_list_s filename_stat =
  let lst_str=List.map (begin_end_P step_k k) file_list_s in
  list_to_file filename_stat lst_str;;

(* 	through_files_P  	1 10 file_list_s filename_stat;;  (*SEMI_READy, W1*) *)

(*  *)
(*  
based on optim_pas_ms2.ml
*)
let rec str_build_Pms step_k k filename=
  let output= find_GPMS filename k  in
  let vo_num= snd output in
  let vo_num_str= string_of_int vo_num in
    if (k>1) then 
      let next_str= str_build_Pms step_k (k-step_k) filename in
      next_str^"\t"^vo_num_str
    else 
      vo_num_str;;

let begin_end_Pms step_k k filename =
    let result= str_build_Pms step_k k filename in
     (* filename^"::\t" ^*)result^"\n";;

let through_files_Pms step_k k file_list_s filename_stat =
  let lst_str=List.map (begin_end_Pms step_k k) file_list_s in
  list_to_file filename_stat lst_str;;

(*       through_files_Pms  	1 10 file_list_s filename_stat;;  *)

(*  
based on optim_path.ml
*)

let rec str_build_STRG step_k k graph path_type_list vo_set=
  let d= voter_intro k graph path_type_list vo_set  in
  let vo_num= hnps d in
  let vo_num_str= string_of_int vo_num in
    if (k>1) then 
      let next_str= str_build_STRG step_k (k-step_k) graph path_type_list vo_set in
      next_str^"\t"^vo_num_str
    else 
      vo_num_str;;

let begin_end_STRG step_k k filename =
    let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
    let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
    let graph= build_graph gate_rec_list in	
    let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
    let all_v= find_all_vertex graph in
    let out_vers=find_out_vert_list all_v vert_names in
    let ini_set = v_list_to_set out_vers in
    let ini_path_type_list= List.map (fun v->{p=[v]; code =2}) 				(VS.elements ini_set)in
    let vo_set=ini_set in
    let result= str_build_STRG step_k k graph ini_path_type_list vo_set in
     (* filename^"::\t" ^*)result^"\n";;

let through_files_STRG step_k k file_list_s filename_stat =
  let lst_str=List.map (begin_end_STRG step_k k) file_list_s in
  list_to_file filename_stat lst_str;;

(* libr for pascal algorithm *)

(*let rec str_build_P step_k k graph =
  let d= voter_intro_P k graph in
  let vo_num= List.length d in
  let vo_num_str= string_of_int vo_num in
    if (k>1) then 
      let next_str= str_build_P step_k (k-step_k) graph   in
      next_str^"\t"^vo_num_str
    else 
      vo_num_str;;

let begin_end_P step_k k filename =
    let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
    let pr_inps_list= inputs_for_n_circuit filename 1 "inputs" in
    let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
    let graph= build_graph gate_rec_list in	
    (*let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
    let all_v= find_all_vertex graph in
    let out_vers=find_out_vert_list all_v vert_names in
    let ini_set = v_list_to_set out_vers in
    let ini_path_type_list= List.map (fun v->{p=[v]; code =2}) 				(VS.elements ini_set)in
    let vo_set=VS.empty in*)
    let result= str_build_P step_k k graph   in
     (* filename^"::\t" ^*)result^"\n";;

let through_files_P step_k k file_list_s filename_stat =
  let lst_str=List.map (begin_end_P step_k k) file_list_s in
  list_to_file filename_stat lst_str;;*)

(* #use "optim_pas_ms.ml";; *)
(*
let rec str_build_Pms step_k k graph =
  let d= voter_intro_Pms k graph in
  let vo_num= List.length d in
  let vo_num_str= string_of_int vo_num in
    if (k>1) then 
      let next_str= str_build_P step_k (k-step_k) graph   in
      next_str^"\t"^vo_num_str
    else 
      vo_num_str;;

let begin_end_Pms step_k k filename =
    let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
    let pr_inps_list= inputs_for_n_circuit filename 1 "inputs" in
    let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
    let graph= build_graph gate_rec_list in	
    let result= str_build_Pms step_k k graph   in
     (* filename^"::\t" ^*)result^"\n";;

let through_files_Pms step_k k file_list_s filename_stat =
  let lst_str=List.map (begin_end_Pms step_k k) file_list_s in
  list_to_file filename_stat lst_str;;*)





