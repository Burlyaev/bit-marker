type v_num = {mutable v_f:G.vertex; mutable n_f: int };;

(* TAIL-RECURSIVE  - manually checked*)
let rec hop_k_paths_list_from_pathes_with_cyc k_hops graph path_type_list   =
  let new_path_type_list= hop_1_time_paths_list_from_pathes graph path_type_list VS.empty in
  let path_lst_wv= paths_filter_no_growth new_path_type_list in
    if (k_hops>1) then  
	  hop_k_paths_list_from_pathes_with_cyc  (k_hops-1) graph path_lst_wv  
    else
	path_lst_wv;;


let graph_all_p_ll k graph  =
  let all_v= find_all_vertex graph in
  let all_v_set = v_list_to_set all_v in
  let func  k graph ver= 
    hop_k_paths_list_from_pathes_with_cyc k graph [{p=[ver];code=2}] 
  in
  List.map (func k graph) (VS.elements all_v_set);; 


let graph_all_p_l k graph  = 
  List.concat (graph_all_p_ll k graph  );;

let v_in_path ver path_type =
    let v_list = path_type.p in
    List.exists (fun v -> v=ver) v_list;;


let how_many_belog_to all_p graph ver=	
  let num = ref 0 in
  let func2 ver path_type =
(*    if (List.exists (fun v -> v=ver) (List.tl (List.rev (List.tl path_type.p)))) then *)
  if (List.exists (fun v -> v=ver)  (List.tl path_type.p)) then 
(*      if (v_in_path ver path_type) then  *)
      num:=!num+1 
  in
  List.iter (func2 ver) all_p;
  !num;;

 
let all_v_num_f all_p  graph= 
  let all_v= find_all_vertex graph in
List.map (how_many_belog_to all_p graph) all_v;;



let comb_v_num all_v all_v_num= 
  List.map2 (fun v num -> {v_f=v;n_f=num})  all_v all_v_num;;

(*  *)
let comp_comb comb1 comb2 = 
    comb1.n_f-comb2.n_f;;

let comb_sort comb_v_num_list =
 List.sort comp_comb comb_v_num_list;;
  

let delete_win winner_v all_p_t_lst =
  let v_in_list winner_v p_t =
  (*  let lst=  (List.tl (List.rev (List.tl p_t.p)))  in*)
  let lst=   (List.tl p_t.p)  in
(*      not(List.exists (fun v -> v=winner_v) (List.tl lst)); *)
     List.for_all (fun v -> v<>winner_v) (lst)
  in
  List.filter (v_in_list winner_v) all_p_t_lst;;

let print_p all_p=
  let print_path path=
    List.iter (fun v-> print_string ((G.V.label v)^" ")) path.p;
    print_string "\n"
  in
List.iter print_path all_p;;

(* TAIL-RECURSIVE  - manually checked*)
let rec vo_intr_path graph all_p vo_list =
  let all_v_num = all_v_num_f all_p graph in
  let when_stop all_v_num=
    List.for_all (fun num-> num=0) all_v_num
  in
  let stop= when_stop all_v_num in

  if (not stop) then 
    let all_v= find_all_vertex graph in
    let sorted = comb_sort (comb_v_num all_v all_v_num) in
    let winner_comb= List.hd (List.rev sorted) in 
    let winner_v= winner_comb.v_f in
    let new_all_p= delete_win winner_v all_p in
    let new_vo_list= List.append vo_list [winner_v] in
(* 	  print_string"NEXT ITER PATHs\n"; *)
(* 	  print_string ("WINNER::"^(G.V.label winner_v)^"\n"); *)
(* 	  print_p new_all_p; *)
(* 	  print_string"END ITER PATHs\n"; *)
      vo_intr_path graph new_all_p new_vo_list 
  else 
    vo_list;;
  
let voter_intro_P k graph out_vers=
(*   let all_v= find_all_vertex graph in *)
  let all_p=graph_all_p_l (k) graph in (*K+1 sicne we should not cut K-length pathes*)
(*    print_string"INIT PATHs\n"; *)
(*    print_p all_p; *)
(*   print_string"END INIT PATHs\n"; *)
  let vo_list=[]  in
  let vo_main_alg=vo_intr_path  graph all_p vo_list in
  let vo_main_algS= v_list_to_set vo_main_alg in
 VS.elements (VS.union (vo_main_algS)(v_list_to_set out_vers)) ;;(* WARNING ::check it maybe be in smarter way, extreme point, how to put voter there and if it's necessary*)

let p_print_Pps filename k=
(*  let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
  let graph= build_graph gate_rec_list in*)

  let graphSstruct= build_graph_MEM filename in
  let vert_names=List.map (fun v-> G.V.label v) (VS.elements (snd (snd graphSstruct))) in
  let graph= fst graphSstruct in

  let all_v= find_all_vertex graph in	
  let out_vers=find_out_vert_list all_v vert_names in
  let vo_lst= voter_intro_P k graph out_vers in
  ((List.map (fun v-> G.V.label v) vo_lst), List.length vo_lst);;
    
(* below function find_GPasc probably is not required- DELETE LATER *)
(*let find_GPasc filename k_hops=
  let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
  let graph= build_graph gate_rec_list in
  let all_v= find_all_vertex graph in	
  let out_vers=find_out_vert_list all_v vert_names in
 voter_intro_P k_hops graph out_vers;;*)






































(*(*  *)
let postdeccessor_list graph pred_set =
  let lst_grow = ref [] in
  let func v =
    lst_grow:= (G.succ graph v):: !lst_grow
  in
  VS.iter func pred_set;
  List.concat (List.rev !lst_grow);;

(*  *)
let postdeccessor_set graph pred_set=
  v_list_to_set (postdeccessor_list graph pred_set);;
(*  *)
let hop_1p_paths_list graph path vo_set= (*DONE- not checked*)
let post_set= remove_vot_vert (postdeccessor_set graph ( VS.singleton(List.hd(List.rev path.p)))) vo_set in	(*without voter verteces*)
  if ((VS.cardinal post_set)>1) then 
    List.map (fun path_t -> {p =path_t.p; code=2} ) (path_split path post_set)
  else
  if ((VS.cardinal post_set)=1) then 
   [{ p = List.append path.p (VS.elements post_set); code=2}]
  else
    [{p=path.p; code=1}];;

let rec hop_1p_paths_list_from_pathes graph path_type_list vo_set=
  let func graph vo_set path_type = 
     hop_1p_paths_list graph path_type vo_set 
  in
 List.concat( List.map (func graph vo_set)  path_type_list);;

(* we shoudl find paths backwards and forward *)
let rec hop_kp_paths_list_from_pathes k graph path_type_list ini_path_type_list vo_set=
  let new_p_path_type_list= hop_1p_paths_list_from_pathes graph path_type_list vo_set in
  let path_lst_wv= paths_filter_no_growth new_p_path_type_list in
    if (k>1) then  
	  hop_kp_paths_list_from_pathes (k-1) graph path_lst_wv ini_path_type_list vo_set
    else
      let ends_of_existing_paths = List.map (fun path_type-> {p=[List.hd (List.rev(path_type.p))];code=2}  ) path_lst_wv in
	(ends_of_existing_paths, vo_set);;
  
(*  let (new_vo_set,b)= loop_elim vo_set path_lst_wv in
  if b then
	 hop_k_time_paths_list_from_pathes k graph ini_path_type_list ini_path_type_list new_vo_set
  else*)

(*  *)
let vert_edge_before_after  k graph path_type_list ini_path_type_list vo_set =
    let pred_paths =hop_k_time_paths_list_from_pathes k graph path_type_list ini_path_type_list vo_set  in
    let post_paths=hop_kp_paths_list_from_pathes k graph path_type_list ini_path_type_list vo_set in

    List.append pred_paths post_paths;;*)