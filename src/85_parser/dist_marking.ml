let graph_unmark graph =
  G.Mark.clear graph;;

let one_hop_set graph pr_Vset=
  let all_predL=ref [] in
  let all_predS=ref VS.empty in
  let all_predS_filtered=ref VS.empty in
  let vos_loops = ref VS.empty in

    let preds_one graph ver=
      all_predL:= (G.pred graph ver):: !all_predL
    in
    let check_on_marking all_predS ver=
      if ((G.Mark.get ver)<>0) then 
	begin
	  vos_loops:= VS.union (!vos_loops) (VS.singleton ver);
(* 	  all_predS_filtered:=VS.union (!all_predS_filtered) (VS.remove ver !all_predS) *)
	end
      else
	begin
	  G.Mark.set ver 1;
	  all_predS_filtered:=VS.union (!all_predS_filtered) (VS.singleton ver)
	end
    in
  all_predL:=[];
  all_predS_filtered:= VS.empty;
  VS.iter (preds_one graph) pr_Vset;
  all_predS:=v_list_to_set (List.concat !all_predL);
  VS.iter (check_on_marking all_predS_filtered) !all_predS;
  [!all_predS_filtered;!vos_loops];;

let k_hops_set graph pr_outsS k_hops=
  let pr_Vset= ref VS.empty in
  let nxt_start_Vset= ref VS.empty in
  let vo_set = ref VS.empty in  
  let temp= ref [] in

  pr_Vset:= pr_outsS;
  for i=1 to k_hops do
    temp:=one_hop_set graph !pr_Vset;
    nxt_start_Vset:= List.hd !temp;
    vo_set:=VS.union (!vo_set) (List.hd(List.tl !temp));
    pr_Vset:= !nxt_start_Vset
  done;
  vo_set:=VS.union (!vo_set) (!nxt_start_Vset);
  [!nxt_start_Vset;!vo_set];;

let into_vo_marking graph pr_outsS k_hops=
  let nxt_start_Vset= ref VS.empty in
  let vo_set= ref VS.empty in
  let temp= ref[] in
  graph_unmark graph;
  nxt_start_Vset:= pr_outsS;
  vo_set:=pr_outsS;
  VS.iter (fun v-> G.Mark.set v 1) pr_outsS;
  while ((VS.cardinal !nxt_start_Vset)<>0) do
    temp:=k_hops_set graph (!nxt_start_Vset) k_hops;
    nxt_start_Vset:= List.hd !temp;
    vo_set:=VS.union (!vo_set) (List.hd(List.tl !temp))
  done;
  !vo_set;;

(* 
    temp:=k_hops_set graph (!nxt_start_Vset) k_hops;;
    nxt_start_Vset:= List.hd !temp;;
VS.iter (fun v-> print_string (G.V.label v)) !nxt_start_Vset;;
    vo_set:=VS.union (!vo_set) (List.hd(List.tl !temp));;
VS.iter (fun v-> print_string (G.V.label v)) !vo_set;;

VS.iter (fun v-> print_string (G.V.label v)) !nxt_start_Vset;
VS.iter (fun v-> print_string (G.V.label v)) !vo_set;

List.map (fun v-> (G.V.label v,G.Mark.get v) )(find_all_vertex graph);;

 *)

let vo_intoMASK filename k_hops =
(*  let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
  let graph= build_graph gate_rec_list in*)

  let graphSstruct= build_graph_MEM filename in
  let vert_names=List.map (fun v-> G.V.label v) (VS.elements (snd (snd graphSstruct))) in
  let graph= fst graphSstruct in

  let all_v= find_all_vertex graph in	
  let out_vers=find_out_vert_list all_v vert_names in
  into_vo_marking graph (v_list_to_set out_vers) k_hops;;

let find_Mvo filename k_hops =
  let vo_set=vo_intoMASK filename k_hops in
  let names=ref [] in
  VS.iter (fun v-> names:= List.append (!names) [(G.V.label v)]) vo_set;
  (!names, (List.length !names));;