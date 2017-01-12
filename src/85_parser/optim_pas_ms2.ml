
let indx_fill_k_LIM all_v ass_v_tab ass_v_Ftab k_hops ver_limS=
    let mul_add_oper k_hops ass_v_tab ass_v_Ftab ver=
	let ass_tab = List.assoc ver !ass_v_tab in
	let ass_Ftab = List.assoc ver !ass_v_Ftab in
	let pred_lst= ass_tab.sv_s in
	let succ_lst= ass_Ftab.sv_Fs in
	let accum= ref (-1) in
	let accumF= ref (-1) in
	let summate accum ass_v_tab k_hops pred_ver=
	  accum:=!accum + (one_pred ass_v_tab k_hops pred_ver)
(* 	  inf_p1_c ass_v_Ftab k_hops pred_ver *)
(* 	  print_string "\nAssume::"^(string_of_int !accum)^("\n") *)
	in
	let summateF accumF ass_v_Ftab k_hops succ_ver=
	  accumF:=!accumF + (one_succ ass_v_Ftab k_hops succ_ver);
(* 	  inf_p1_c ass_v_Ftab k_hops pred_ver *)
	in
      accum:=0;
      accumF:=0;
      Array.iter (summate accum ass_v_tab k_hops) pred_lst;
      Array.iter (summateF accumF ass_v_Ftab k_hops) succ_lst;
(* print_string ("Accum:"^(string_of_int !accum)^"\n");(* DEBUGGING*) *)
      Array.set  ass_tab.indx (k_hops) !accum;
      Array.set  ass_Ftab.ctr (k_hops) !accumF
    in
  List.iter (mul_add_oper k_hops ass_v_tab ass_v_Ftab) ver_limS;;





let indx_fill_till_k_LIM  graph all_v ass_v_tab ass_v_Ftab k_hops ver_limS=
  indx_fill_1_LIM graph all_v ass_v_tab ass_v_Ftab ver_limS;
(*   tuning_selfloops all_v ass_v_tab ver_limS; *)
  for i=1 to (k_hops-1) do
(*     print_string ("Interating::"^(string_of_int i)); *)
    indx_fill_k_LIM all_v ass_v_tab ass_v_Ftab i ver_limS;
(*     tuning_loops graph all_v ass_v_tab i ver_limS *)
  done;;





(* (* DEBUGGING SUB-LIBRARY *) *)
let p_p voter_list points_v_tab =
  let func1 ass_v=
      let ver= fst ass_v in
      let ver_name= G.V.label ver in
    print_string (ver_name^"\t::\t"^(string_of_int (snd ass_v)));
	let condition = List.exists (fun vot-> (G.V.label vot)=ver_name) voter_list
	in
      if (condition) then
      print_string " VOT ";
    print_string "\tEND\n "
  in
List.iter func1 points_v_tab;;

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

(* (* DEBUGGING SUB-LIBRARY END*) *)



(* we should delete del_ver from  sv_s and recaulculae indexes in ass_v_tab {and ass_v_Ftab done automatically for k vertexes after del_ver}  *)
let voter_intro_Pms k_hops graph =
  let all_v= find_all_vertex graph in
  let condition_true voter_list points_v_tab =
    let its_NOT_vot voter_list pnt_lst = List.for_all (fun vo-> (G.V.label vo) <> (G.V.label (fst pnt_lst))) voter_list in  
    List.exists (fun pnt_tlp-> ((snd pnt_tlp)<>0)&&(its_NOT_vot voter_list pnt_tlp) )  points_v_tab 
  in
  let win_V all_v win_ver= List.find (fun v->(G.V.label v)= (G.V.label win_ver)) all_v in
  let ass_v_tab= ref[] in
  let ass_v_Ftab= ref [] in
  let points_v_tab= ref [] in
  let voter_list = ref [] in
  let ver_limS = ref VS.empty in
  let b_s = ref VS.empty in
  let f_s = ref VS.empty in
  let win_ver= ref [] in
  let del_ver= ref [] in

  ass_v_tab := dum_eV all_v;
  ass_v_Ftab := dum_frw_counters all_v;
(* we created empty counter-sv tables*)
  fill_succ_pred ass_v_tab ass_v_Ftab graph all_v;
  create_k_CounBaF ass_v_tab ass_v_Ftab all_v (k_hops);
(* changed from K to (K+1) since we don't have to cut k hops if FM SEU(1,K) *)
  indx_fill_till_k_LIM graph  all_v ass_v_tab ass_v_Ftab (k_hops) all_v;
(* changed from K to (K+1), see reasoning above *)
(* we put all indexes to all verteces   *)
  points_v_tab := v_p_tab all_v ass_v_tab ass_v_Ftab (k_hops);
  voter_list:=[];
(*    p_p !voter_list !points_v_tab;
    p_t !ass_v_tab;
    p_Ft !ass_v_Ftab; *)
(* print_string "While STARTED\n"; *)
  while (condition_true !voter_list !points_v_tab) do
    win_ver :=	 [choose_winner voter_list !points_v_tab];
    voter_list:= List.append !voter_list !win_ver;
  (* we identify the candidate to put the voter *)
(* DEBUGGING  *)
(*     p_p !voter_list !points_v_tab; *)
(*     print_string ("\n Chosen:: "^(G.V.label (List.hd !win_ver))^"\n\n"); *)
(*     p_t !ass_v_tab; *)
(*     p_Ft !ass_v_Ftab; *)
(* DEBUGGING *)
    del_ver := [win_V all_v (List.hd !win_ver)];
    exc_ver_sv  graph ass_v_tab ass_v_Ftab (List.hd !del_ver); (* we delete this vertex from the predicessor lists of all other V *)
    
    b_s:=bck_set_to_update graph ass_v_Ftab (k_hops) (List.hd !del_ver);

(*	print_string "Set behind me::\n";
	VS.iter (fun v-> print_string ((G.V.label v)^"; ")) !b_s;
	print_string " ::Set ended\n";*)

    f_s:=fwd_set_to_update graph ass_v_Ftab (k_hops) (List.hd !del_ver);

(*	print_string "Set in front of me::";
	VS.iter (fun v-> print_string ((G.V.label v)^"; ")) !f_s;
	print_string " ::Set ended\n";*)

    ver_limS:=VS.union (VS.union !b_s !f_s) (VS.singleton (List.hd !del_ver));

(*     clean_bck_set graph ass_v_Ftab k_hops del_ver;(* we clean the forward tables of  behind me- to include them again *) *)
(*     ver_limS :=fwd_set_to_update graph ass_v_Ftab k_hops del_ver;(* we identify the V set in front of me to update their tab *) *)
    indx_fill_till_k_LIM graph all_v ass_v_tab ass_v_Ftab (k_hops) (VS.elements !ver_limS);
    points_v_tab := v_p_tab all_v ass_v_tab ass_v_Ftab (k_hops);
  done;
(* print_string "While STARTED DONE\n"; *)
  !voter_list;;

let find_GPMS filename k_hops=
(*  let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
  let graph= build_graph gate_rec_list in*)

  let graphSstruct= build_graph_MEM filename in
  let vert_names=List.map (fun v-> G.V.label v) (VS.elements (snd (snd graphSstruct))) in
  let graph= fst graphSstruct in

  let all_v= find_all_vertex graph in	
  let out_vers=find_out_vert_list all_v vert_names in
  let vo_norm=voter_intro_Pms k_hops graph in
  let all_vo = VS.union (v_list_to_set vo_norm) (v_list_to_set out_vers) in
  let vo_lst=(VS.elements all_vo)  in
  (vo_lst, List.length vo_lst);;



let nvl all_v=
  List.map (fun el-> G.V.label el) all_v;;


