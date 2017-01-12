
type back_arr_1 = {mutable sv_s: G.vertex array; mutable indx: int array };;
type frw_counters= {mutable sv_Fs: G.vertex array; mutable ctr: int array};;

(* intially this function was created for lists not for arrays - the only change is 
from [] to [||] *)
let dum_eV all_v=
  let cr_dumBL ver=
    (ver,{sv_s= [||];indx=[||]}) 
  in
List.map (cr_dumBL) all_v;;

let dum_frw_counters all_v=
  let cr_dumC ver=
    (ver,{sv_Fs= [||];ctr=[||]}) 
  in
List.map (cr_dumC) all_v;;

(* let ass_v_tab = dum_eV all_v;;(* TEST *) *)
(* let ass_v_Ftab= dum_frw_counters all_v;;(* TEST *) *)
					(*let build_assoc all_v =
					  let tab_BL=dum_eV all_v in
					  List.combine all_v tab_BL;;*)
let zero_vec k= 
  let out=ref [] in
  for i=1 to k do
    out := List.append !out [0]
  done;
  !out;;

(* the above function is created for the list - for the arrays the function is embedded: "Array.make x 0" *)

let one_pred_v_lst ass_v_tab_ref graph ver = 
    let succ_list= G.pred graph ver in
    let ass_table = List.assoc ver !ass_v_tab_ref in 
      ass_table.sv_s <- Array.of_list succ_list;;

let one_succ_v_lst ass_v_Ftab_ref graph ver = 
    let pred_list= G.succ graph ver in
    let ass_table = List.assoc ver !ass_v_Ftab_ref in 
      ass_table.sv_Fs <- Array.of_list pred_list;;
     

let fill_succ_pred ass_v_tab_ref ass_v_Ftab_ref graph all_v= 
  List.iter (one_pred_v_lst ass_v_tab_ref graph) all_v;
  List.iter (one_succ_v_lst ass_v_Ftab_ref graph) all_v;;

(* fill_succ ass_v_tab graph all_v;;(* TEST *) *)


let create_k_CounBaF ass_v_tab ass_v_Ftab all_v fm1_k=
  let fun1 ass_v_tab ass_v_Ftab ver=
      let ass_tab= List.assoc ver !ass_v_tab in
      let ass_Ftab= List.assoc ver !ass_v_Ftab in
    ass_tab.indx<-Array.make fm1_k 0;
    ass_Ftab.ctr <- Array.make fm1_k 0
  in
  List.iter (fun1  ass_v_tab ass_v_Ftab ) all_v;;

(* create_k_CounBaF ass_v_tab ass_v_Ftab all_v 2;;(* TEST *) *)

		      (*let inf_p1_c ass_v_Ftab k_hops ver =
			  let the_counter_list= List.assoc ver !ass_v_Ftab in
			  let the_counter_k= Array.get the_counter_list.ctr (k_hops-1) in
			  Array.set the_counter_list.ctr (k_hops-1) (the_counter_k+1);;

		      let inf_pk_c ass_v_Ftab k_hops ver =
			  let the_counter_list= List.assoc ver !ass_v_Ftab in
			  let the_counter_k= Array.get the_counter_list.ctr (k_hops-1) in
			Array.set the_counter_list.ctr (k_hops-1) (the_counter_k+1);;*)

(* the function below counts the number of pathes of the length (k+1) the vertex belongs to
which is done for FM SEU(1,K). To count pathes fof the lengths K does not make sence, since
we should not cut the path e.g. of length 2 if SEU (1,2). To count the total numbfer of patheses
of the length >k, currently have no clue how without path saving*)

let build_points all_v ass_v_tab ass_v_Ftab k_hops =
  let count_points ass_v_tab ass_v_Ftab k_hops ver=
    let assoc_tab= List.assoc ver !ass_v_tab in
    let assoc_Ftab= List.assoc ver !ass_v_Ftab in
    let points= ref 0 in
    points:=0;

    (*points:=assoc_tab.indx.(k_hops-1)+assoc_Ftab.ctr.(k_hops-1);*)
    points:=assoc_Ftab.ctr.(k_hops-1); (*chnaged to keep correspondence with non-optimized version*)
(*      let level k_hops num = if (num>k_hops) num else 0 in
      points:=(level k_hops assoc_tab.indx.(k_hops))+(level k_hops assoc_Ftab.ctr.(k_hops))*)
(*     print_string ("Extreme cases"^(string_of_int assoc_tab.indx.(k_hops-1))^"*"^(string_of_int assoc_Ftab.ctr.(k_hops-1))^"\n"); *)
    if (k_hops>1) then
      for i=0 to (k_hops-2) do
	    points:=!points+( assoc_tab.indx.(i)*assoc_Ftab.ctr.(k_hops-i-2))
	 
    (*       print_string ("I am multiplying"^(string_of_int assoc_tab.indx.(i-1))^"*"^(string_of_int assoc_Ftab.ctr.(k_hops-i-1))^"\n") *)
	
      done;
   
    !points
  in
  List.map (count_points ass_v_tab ass_v_Ftab k_hops) all_v;;


let v_p_tab all_v ass_v_tab ass_v_Ftab k_hops=
  let pnt_lst= build_points all_v ass_v_tab ass_v_Ftab k_hops in
  List.combine all_v pnt_lst;;

(* let points_v_tab= v_p_tab all_v ass_v_tab ass_v_Ftab 2;;(* TEST *) *)

let choose_winner voter_list points_v_tab =
(*  let compar_fun tup1 tup2=
    (snd tup2) -  (snd tup1)
  in*)
  let sorted=List.sort compar_fun points_v_tab in 
(*   let win_ver= ref [] in *)
  let fun1 voter_list ver=
    List.for_all (fun vo->(G.V.label (fst ver) )<>(G.V.label vo)) !voter_list
  in
(*   fst (List.hd (sorted));; *)
  let win_ver= fst (List.find (fun1 voter_list) sorted) in
  win_ver;;

let rem_el_array arr exc_el =    
    let new_arr= ref [||] in
    let build_n_arr new_arr exc_el el_arr=
      if (exc_el<>el_arr) then
	new_arr:= Array.append [|el_arr|] !new_arr
    in
    new_arr:=[||];
    Array.iter (build_n_arr new_arr exc_el) arr;
    !new_arr;;

let exc_ver_sv  graph ass_v_tab ass_v_Ftab del_ver=
  let succ_lst= G.succ graph del_ver in
  let pred_lst= G.pred graph del_ver in

  let del_sv_s del_ver ass_v_tab succ_ver=
      let assoc_tab= List.assoc succ_ver !ass_v_tab in
      assoc_tab.sv_s <- rem_el_array assoc_tab.sv_s del_ver
(* 	print_string ( G.V.label assoc_tab.sv_s.(0) ) *)
  in

  let del_sv_Fs del_ver ass_v_Ftab pred_ver=
    let assoc_Ftab= List.assoc pred_ver !ass_v_Ftab in
    assoc_Ftab.sv_Fs <- rem_el_array assoc_Ftab.sv_Fs del_ver
(*    print_string ( G.V.label assoc_Ftab.sv_Fs.(0) ) *)
  in
  List.iter (del_sv_s del_ver ass_v_tab) succ_lst;
  List.iter (del_sv_Fs del_ver ass_v_Ftab) pred_lst;;


let set_minus set1 set2=
  VS.filter (fun el_s1-> VS.for_all (fun el_s2-> el_s2<>el_s1) set2) set1;;


let fwd_set_to_update graph ass_v_Ftab k_hops del_ver=
    let fwd_set= ref VS.empty in
    let nxt_iter =ref VS.empty in
    let now_iter =ref VS.empty in
  fwd_set	:= VS.empty;
  now_iter	:= VS.union !fwd_set (VS.singleton del_ver);
  for i=1 to k_hops do 
    nxt_iter	:= VS.empty;
      let add_succ_one graph nxt_iter now=
	nxt_iter := VS.union (!nxt_iter) (v_list_to_set(G.succ graph now)) 
      in
    VS.iter (add_succ_one graph nxt_iter)  !now_iter;
    now_iter := set_minus (!nxt_iter) (!fwd_set);
    fwd_set := VS.union (!fwd_set) (!now_iter)
  done;
  !fwd_set;;

let bck_set_to_update graph ass_v_Ftab k_hops del_ver=
    let bck_set= ref VS.empty in
    let nxt_iter =ref VS.empty in
    let now_iter =ref VS.empty in
  bck_set	:= VS.empty;
  now_iter	:= VS.union (!bck_set) (VS.singleton del_ver);
  for i=1 to k_hops do 
    nxt_iter	:= VS.empty;
      let add_pred_one graph nxt_iter now=
	nxt_iter := VS.union (!nxt_iter) (v_list_to_set(G.pred graph now)) 
      in
    VS.iter (add_pred_one graph nxt_iter)  !now_iter;
    now_iter := set_minus (!nxt_iter) (!bck_set);
    bck_set := VS.union (!bck_set) (!now_iter)
  done;
  !bck_set;;

let clean_bck_set graph ass_v_Ftab k_hops del_ver=
  let bck_set= bck_set_to_update graph ass_v_Ftab k_hops del_ver in
  let cln_Ftab ass_v_Ftab ver =
    let assoc_Ftab= List.assoc ver !ass_v_Ftab in
    assoc_Ftab.ctr<- Array.make (Array.length assoc_Ftab.ctr) 0
  in
  VS.iter (cln_Ftab ass_v_Ftab) bck_set;;


let one_pred ass_v_tab k_hops pred_ver =
  let assoc_tab= List.assoc  pred_ver !ass_v_tab in
(* 		  print_string ("\tone_pred:"^(string_of_int (Array.get assoc_tab.indx (k_hops-1)))^"\n");(* DEBUGGING*) *)
(* 		  print_string ("k_hops::"^(string_of_int (k_hops-1))^"\n"); *)
  Array.get assoc_tab.indx (k_hops-1);;

let one_succ ass_v_Ftab k_hops succ_ver =
  let assoc_tab= List.assoc  succ_ver !ass_v_Ftab in
(* 		  print_string ("\tone_pred:"^(string_of_int (Array.get assoc_tab.ctr (k_hops-1)))^"\n");(* DEBUGGING*) *)
(* 		  print_string ("k_hops::"^(string_of_int (k_hops-1))^"\n"); *)
  Array.get assoc_tab.ctr (k_hops-1);;


let indx_fill_1_LIM graph all_v ass_v_tab ass_v_Ftab ver_limS=
  let fill_int graph ass_v_tab ass_v_Ftab ver=
      let ass_tab = List.assoc ver !ass_v_tab in
      let ass_Ftab = List.assoc ver !ass_v_Ftab in
      Array.set ass_tab.indx 0 (Array.length ass_tab.sv_s);(*DON'TOUCH- CORRECT*)
(*       Array.set ass_Ftab.ctr 0 (Array.length ass_Ftab.sv_Fs); *)
(*   Array.set ass_tab.indx 0 (List.length (G.pred graph ver)); *)
      Array.set ass_Ftab.ctr 0 (List.length (G.succ graph ver)); (*DON'TOUCH- CORRECT*)
(*       Array.iter (inf_p1_c ass_v_Ftab 1 ) ass_tab.sv_s *)
  in
  List.iter (fill_int graph ass_v_tab ass_v_Ftab) ver_limS;;

  
