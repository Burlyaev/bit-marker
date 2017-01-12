(* WORKING VERSION 
Last version: 27.02.2013, 15:19*)

(* TAIL-RECURSIVE - changed (re-written) *)
let pow n x =
  let rec pow_TL n x accumulator =
    match n with
      |0->accumulator
      |_->let accum= accumulator*x in
	  pow_TL (n-1) x accum;
  in
pow_TL n x 1;;

(*let rec pow n x =
  if n=0 then 
    1 
  else 
    x*(pow (n-1) x);;*)

type v_hop = {mutable vtx: G.vertex ; mutable hops: int};;

let one_hop_B graph ver =
  let one_hop_B = G.pred graph ver in
  let one_hop_B_NV = List.filter (fun v-> ((G.Mark.get v) <>1)) one_hop_B in
  one_hop_B_NV;;

let one_hop_F graph ver =
  let one_hop_F = G.succ graph ver in
  let one_hop_F_NV = List.filter (fun v-> ((G.Mark.get v) <>1)) one_hop_F in
  one_hop_F_NV;;

let one_hop_BS graph verS=
  let hop_BS= ref VS.empty in
  let temp_B= ref [] in
  let for_one_ver graph ver=
     temp_B:=one_hop_B graph ver;
     hop_BS:= VS.union (v_list_to_set(!temp_B)) (!hop_BS);
  in
  hop_BS:=VS.empty;
  VS.iter (for_one_ver graph) verS;
  !hop_BS;;

let one_hop_FS graph verS=
  let hop_FS= ref VS.empty in
  let temp_F= ref [] in
  let for_one_ver graph ver=
     temp_F:=one_hop_F graph ver;
     hop_FS:= VS.union (v_list_to_set(!temp_F)) (!hop_FS)
  in
  hop_FS:=VS.empty;
  VS.iter (for_one_ver graph) verS;
  !hop_FS;;

let k_hops_BF graph start_ver k_hops =
  let k_hop_B= ref VS.empty in
  let k_hop_F= ref VS.empty in

  k_hop_B:=VS.singleton start_ver;
  k_hop_F:=VS.singleton start_ver;  
  for i=1 to k_hops do
    k_hop_B :=one_hop_BS graph !k_hop_B;
    k_hop_F :=one_hop_FS graph !k_hop_F;
  done;
  [!k_hop_B;!k_hop_F];;

let if_vo_ERR graph k_hops start_ver =	
  let set_list= k_hops_BF graph start_ver k_hops in
  let b= List.nth set_list 0 in
  let f=List.nth set_list 1 in 
  ((VS.cardinal b)<>0)||((VS.cardinal f)<>0);;

let check_valid graph k_hops =
  let all_v= find_all_vertex graph in
  let all_voL= List.filter (fun v -> ((G.Mark.get v)<>0))all_v in
  let condition = ref true in
  condition:=true;
  if (( List.length all_voL) =0) then condition:=false;
  if (List.exists (fun start_ver->if_vo_ERR graph k_hops start_ver) all_voL) then condition:=false;
  !condition;;
  

(* extensive search function for the optimal voter insertion *)
let ith_posBI bi i=
  let shifted_BI= Big_int.shift_right_big_int bi i in
  let bit_BI=Big_int.extract_big_int shifted_BI 1 1 in
  Big_int.int_of_big_int bit_BI;;


let mark_int_pattern graph outsS patt_int=
  let all_v= find_all_vertex graph in
  let all_vF= List.filter (fun ver-> VS.for_all (fun verS -> (G.V.label ver)<> (G.V.label verS)) outsS ) all_v in
let binStr = BatBig_int.to_string_in_binary patt_int in
(*   let binStr= binStr_of_decInt patt_int in *)
  graph_unmark graph;
  for i=0 to ((String.length binStr)-1) do
    if ((String.get binStr (((String.length binStr)-1)-i))<>'0') then
      G.Mark.set (List.nth all_vF i) 1
  done;;
(*   graph;; *)

let how_many_ones patt_int=
let binStr = BatBig_int.to_string_in_binary patt_int in
(*   let binStr= binStr_of_decInt patt_int in *)
  let num_ones= ref 0 in
  num_ones:=0;
  String.iter (fun c-> if (c='1') then num_ones:=(!num_ones+1)) binStr;
  !num_ones;;

(*let find_Gopt graph k_hops = 
  let all_v= find_all_vertex graph in
  let bin_pos_num= List.length all_v in
 (* let graph_ref = ref [] in*)
  let g_winner= ref [] in
  let vo_num_win=  ref 0 in
  let vo_num_suspect = ref 0 in
  
  vo_num_win:=bin_pos_num;
  for i=1 to ((pow bin_pos_num 2)-1) do
    mark_int_pattern graph i;
    vo_num_suspect:= how_many_ones i;
    if ((!vo_num_suspect)<=(!vo_num_win)) then
      begin(*CAN BE SEV.WINNERS!!*)
	if (check_valid graph k_hops) then 
	  begin
	    g_winner:=[(G.copy graph)];
	    mark_int_pattern (List.hd !g_winner) i;
(* 	      print_string"\n START WINNER\n"; *)
(* 	      print_string ("Pattern:: "^(binStr_of_decInt i)^"\n"); *)
(* 	      List.iter (fun v -> if ((G.Mark.get v)>0) then print_string (G.V.label v)) all_v; *)
(* 	      print_string"END WINNER\n"; *)
	    vo_num_win:=!vo_num_suspect
	  end;
      end;
  done;
  (List.hd !g_winner, !vo_num_win);;*)

let mark_outs outsS graph =
  VS.iter (fun v -> G.Mark.set v 1) outsS;;
  



let find_Gopt graph outsS k_hops = 
  let all_v= find_all_vertex graph in
  let bin_pos_num= ((List.length all_v)-(VS.cardinal outsS)) in
 (* let graph_ref = ref [] in*)
  let g_winner= ref [] in
  let vo_num_win=  ref 0 in
  let vo_num_suspect = ref 0 in
  let iNUM_BN= ref BatBig_int.zero in
  
  vo_num_win:=bin_pos_num;
  iNUM_BN:= BatBig_int.pred (BatBig_int.power_int_positive_int 2 bin_pos_num);

(*  Big_int.string_of_big_int (Big_int.power_int_positive_int 2 10);; *)
(*   for iter=1 to (!iNUM_BN) do *)
  while (not(BatBig_int.eq_big_int (!iNUM_BN) (BatBig_int.unit_big_int))) do 
    mark_int_pattern graph outsS !iNUM_BN; (*contains mark_clean procedure*)
    mark_outs outsS graph;
    vo_num_suspect:= how_many_ones !iNUM_BN;
(*     print_string ("Itteration::"^(BatBig_int.string_of_big_int (!iNUM_BN))^"\n"); *)
    if ((!vo_num_suspect)<=(!vo_num_win)) then
      begin(*CAN BE SEV.WINNERS!!*)
	if (check_valid graph k_hops) then 
	  begin
	    g_winner:=[(G.copy graph)];
	    mark_int_pattern (List.hd !g_winner) outsS !iNUM_BN;
	    mark_outs outsS (List.hd !g_winner);
(* 	      print_string"\n START WINNER\n"; *)
(* 	      print_string ("Pattern:: "^(binStr_of_decInt iter)^"\n"); *)
(* 	      List.iter (fun v -> if ((G.Mark.get v)>0) then print_string (G.V.label v)) all_v; *)
(* 	      print_string"END WINNER\n"; *)
	    vo_num_win:=!vo_num_suspect
	  end;
      end;
    iNUM_BN:= Big_int.pred_big_int !iNUM_BN
  done;
  (List.hd !g_winner, (!vo_num_win+(VS.cardinal outsS)));;

let find_GoptOUT filename k_hops=
(*  let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
  let graph= build_graph gate_rec_list in*)
  let graphSstruct= build_graph_MEM filename in
  let vert_names=List.map (fun v-> G.V.label v) (VS.elements (snd (snd graphSstruct))) in
  let graph= fst graphSstruct in

  let all_v= find_all_vertex graph in	
  let out_vers=find_out_vert_list all_v vert_names in
find_Gopt graph (v_list_to_set out_vers) k_hops;;

(* DEBUGGIN *)
(*  List.map (fun v -> G.Mark.get v) all_v;; *)
