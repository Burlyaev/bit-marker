type res_of_hop = {mutable p:VS.elt list; mutable code: int };;

let path_split path succ_set=
  List.map (fun v-> {p= List.append path.p [v];code=2}) (VS.elements succ_set);;

(* code 1 - no successors/either run into voter *)
(* code 2 - others *)
(* code 3 - loop is indentified during the last hop step *)
(*  *)

let hop_1_time_paths_list graph path vo_set= (*DONE- not checked*)
  let last_elem= List.hd (List.rev path.p) in
  let preds= G.pred graph last_elem in
(*   print_string ((string_of_int (List.length preds))^"--preds"); *)
  match (List.length preds) with 
    | 0 -> [{p=path.p; code=1}]
    | _ -> 
	List.map (fun pred-> {p=List.append path.p [pred];code=2}) preds
  ;;
    


(*let hop_1_time_paths_list graph path vo_set= (*DONE- not checked*)
let suc_set= remove_vot_vert (successor_set graph ( VS.singleton(List.hd(List.rev path.p)))) vo_set in	(*without voter verteces*)
  if ((VS.cardinal suc_set)>1) then 
(*     List.map (fun path_t -> {p =path_t.p; code=2} ) (path_split path suc_set) *)
      path_split path suc_set
  else
  if ((VS.cardinal suc_set)=1) then 
   [{ p = List.append path.p (VS.elements suc_set); code=2}]
  else
    [{p=path.p; code=1}];;*)

(*  *)
let paths_filter_no_growth path_type_list =
  List.filter (fun path_type->  not (path_type.code=1)) path_type_list;;

(*  *)
let loops_find_after_1_hop path_type_list = (*DONE- not checked*)
  let func path_type=
    if (List.exists (fun v-> v=(List.hd (List.rev path_type.p)))	(List.tl (List.rev path_type.p))) then
      {p=path_type.p; code =3}
    else
      {p=path_type.p;code=path_type.code}
  in
  List.map  func  path_type_list;;

(* (*  *) *)
  let np type_path= List.map (fun v-> G.V.label v ) type_path.p;; (*DEBUGGING FUNC*)
  let nps v_set= List.map (fun v-> G.V.label v ) (VS.elements v_set);; (*DEBUGGING FUNC*)
  let hnps d = List.length (nps  d) ;;
(*  *)
  let npl type_path_list k= 
      let path_type= List.nth type_path_list k in
    List.map (fun v-> G.V.label v ) path_type.p;; (*DEBUGGING FUNC*)
(* (*  *) *)
(* MODIF- "rec" deeted since it's not recursive function *)
let hop_1_time_paths_list_from_pathes graph path_type_list vo_set=
  let func graph vo_set path_type = 
     hop_1_time_paths_list graph path_type vo_set 
  in
 List.concat( List.map (func graph vo_set)  path_type_list);;

let loop_elim vo_set path_type_list =
   let new_vo_set= ref VS.empty in
   let mark3_loops= loops_find_after_1_hop path_type_list in
   let filter_loops = List.filter (fun path_type -> path_type.code=3) mark3_loops in
   new_vo_set:=vo_set;
   if ((List.length filter_loops)=0) then 
    (vo_set,false)
   else 
    begin
(*       let choosen_loop = List.hd filter_loops in (*just choose any loop - they first one for ex*) *)
(*       let new_vo_set:=(VS.union (!new_vo_set) (VS.singleton (List.hd(List.rev choosen_loop.p)))) in *)
      List.iter (fun ch_loop-> 
	  new_vo_set:=VS.union (!new_vo_set) (VS.singleton (List.hd(List.rev ch_loop.p))) )
		  filter_loops;
    (!new_vo_set,true)
    end;;

(* TAIL-RECIRSUVE - manually checked     *)
let rec hop_k_time_paths_list_from_pathes k graph path_type_list ini_path_type_list vo_set=
  let new_path_type_list= hop_1_time_paths_list_from_pathes graph path_type_list vo_set in
  let path_lst_wv= paths_filter_no_growth new_path_type_list in
  let (new_vo_set,b)= loop_elim vo_set path_lst_wv in
  if b then
	 hop_k_time_paths_list_from_pathes k graph ini_path_type_list ini_path_type_list new_vo_set
  else
    if (k>1) then  
	  hop_k_time_paths_list_from_pathes (k-1) graph path_lst_wv ini_path_type_list new_vo_set
    else
      let ends_of_existing_paths = List.map (fun path_type-> {p=[List.hd (List.rev(path_type.p))];code=2}  ) path_lst_wv in
	(ends_of_existing_paths, new_vo_set);;

(* TAIL-RECIRSUVE - manually checked *)
let rec voter_intro_STRG k graph path_type_list vo_set=
  let (ends_of_k_paths,new_vo_set) = hop_k_time_paths_list_from_pathes k graph path_type_list path_type_list vo_set in 
  let vo_sub_last_this_iter = (VS.diff  new_vo_set vo_set) in

    if (((List.length ends_of_k_paths)>0)||((VS.cardinal vo_sub_last_this_iter)>0)) then
(*      let places_arr_path_type= List.map (fun path_type-> {p= [List.hd(List.rev(path_type.p))]; code=2}) ends_of_k_paths in*)
      let places_arr_path_type= ends_of_k_paths in
      let start_paths_after_new_vo=  List.map (fun v->{p=[v]; code =2}) (VS.elements vo_sub_last_this_iter) in
      let places_arr_set= v_list_to_set (List.map (fun path_type-> List.hd(path_type.p)) places_arr_path_type) in
      let new_vo_set_cyc_ends= (VS.union new_vo_set places_arr_set) in 
      let all_paths_start_with= List.append  places_arr_path_type start_paths_after_new_vo in
      voter_intro_STRG  k graph all_paths_start_with new_vo_set_cyc_ends
    else
	(VS.elements vo_set, VS.cardinal vo_set);;

(*  *)
let choose_win_path path_type_list =
    let path_lgthT= List.map (fun path_type -> (path_type,(List.length path_type.p))) path_type_list in
    let sorted=List.sort compar_fun path_lgthT in 
  List.hd sorted;;

(*        through_files_STRG  	1 10 file_list_s filename_stat;;   *)

let find_STRG filename k_hops=
(*    let gate_rec_list = no_internal_dff (pure_circuit_sub filename) in
    let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
    let graph= build_graph gate_rec_list in	
    let vert_names=find_out_vert_names gate_rec_list pr_outs_list in*)

    let graphSstruct= build_graph_MEM filename in
    let vert_names=List.map (fun v-> G.V.label v) (VS.elements (snd (snd graphSstruct))) in
    let graph= fst graphSstruct in
    
    let all_v= find_all_vertex graph in
    let out_vers=find_out_vert_list all_v vert_names in
    let ini_set = v_list_to_set out_vers in
    let ini_path_type_list= List.map (fun v->{p=[v]; code =2}) (VS.elements ini_set) in
    let vo_set=ini_set in
    let result=voter_intro_STRG k_hops graph ini_path_type_list vo_set in
  result;;
