(*  
patially started but not finished, it was more or less evident that such approach
may lead to infinite cycle in recomputation since 2 lines may run into each other and force the tool to reconsider one other. At this point- thinking about this algorithm is suspended
*)


let one_hopSREC graph pr_Vset nUM=
  let all_predL=ref [] in
  let all_predS=ref VS.empty in
  let vos_loops = ref VS.empty in
    let preds_one graph ver=
      all_predL:= (G.pred graph ver):: !all_predL
    in
    let check_on_marking ver=
      match (G.Mark.get ver) with
	  | 0	-> 	G.Mark.set ver nUM
	  | _	->vos_loops:= VS.union (!vos_loops) (VS.singleton ver)
    in
  all_predL:=[];
  VS.iter (preds_one graph) pr_Vset;
  all_predS:=v_list_to_set (List.concat !all_predL);
  VS.iter (check_on_marking) !all_predS;
  [!all_predS;!vos_loops];;



(*match (a) with
	  | 0	-> 	print_string "0"
	  | _	->	print_string "1"*)


let k_hops_setREC graph pr_outsS k_hops=
  let pr_Vset= ref VS.empty in
  let nxt_start_Vset= ref VS.empty in
  let vo_set = ref VS.empty in  
  let temp= ref [] in
  pr_Vset:= pr_outsS;
  for i=1 to k_hops do
    temp:=one_hop_set graph !pr_Vset;
    nxt_start_Vset:= List.hd !temp;
    vo_set:=List.hd(List.tl !temp);
    pr_Vset:= !nxt_start_Vset
  done;
  vo_set:=VS.union (!vo_set) (!nxt_start_Vset);
  [!nxt_start_Vset;!vo_set];;

let into_vo_markingREC graph pr_outsS k_hops=
  let nxt_start_Vset= ref VS.empty in
  let vo_set= ref VS.empty in
  let temp= ref[] in

  graph_unmark graph;
  nxt_start_Vset:= pr_outsS;
  while ((VS.cardinal !nxt_start_Vset)<>0) do
    temp:=k_hops_setREC graph (!nxt_start_Vset) k_hops;
    nxt_start_Vset:= List.hd !temp;
    vo_set:=List.hd(List.tl !temp)
  done;
  !vo_set;;

let vo_intoMASKREC k_hops filename=
  let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
  let out_vers=find_out_vert_list all_v vert_names in
  let graph= build_graph gate_rec_list in
  into_vo_marking graph (v_list_to_set out_vers) k_hops;;
