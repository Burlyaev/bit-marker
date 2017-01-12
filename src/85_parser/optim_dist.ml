(*  
READY: 27.02.1013, 16:26
*)
let find_out_vert_recs gate_rec_list pr_outs_list= 
  List.filter (fun record-> (List.exists (fun str-> str=record.out) pr_outs_list)) gate_rec_list;;
(*  *)
let find_out_vert_names gate_rec_list pr_outs_list=
  let recs=find_out_vert_recs gate_rec_list pr_outs_list in
  List.map (fun record-> record.name) recs;;
(*  *)
let find_all_vertex graph =
  let all_vertexes = ref[] in
  let func  vert = 
   all_vertexes := vert  :: !all_vertexes
  in
  G.iter_vertex func graph;
  List.rev !all_vertexes ;;
(*  *)
let find_out_vert_list all_v vert_names=
  List.filter (fun v-> List.exists (fun name -> (G.V.label v)=name) vert_names) all_v;;
(*  *)
(*let if_mem_cell v gate_rec_list =
  let v_name= G.V.label v in
  List.exists (fun record -> (record.name = v_name)&&(record.op= "DFFX")) gate_rec_list;;*)
(*  *)
(*let marking n_v gate_rec_list pred_v k=
	  let if_mem = (if_mem_cell n_v gate_rec_list) in
	  let mark_of_prev_v= G.Mark.get pred_v in

	  if ((if_mem)&((G.Mark.get n_v)=0)) then
	    G.Mark.set n_v (mark_of_prev_v+1)  
	  else

	  if ((not(if_mem))&((G.Mark.get n_v)=0)) then
	    G.Mark.set n_v (mark_of_prev_v+0)
	  else 

	  if (((G.Mark.get n_v)<>0)&(if_mem)) then
	      G.Mark.set n_v k
	  else

	  if (((G.Mark.get n_v)<>0)&(not(if_mem)))then 
	  mark_k_last_mem ;;*)
(* hereafter the working correct version*)
(*  *)
   
(*  *)
let successor_list graph pred_set =
  let lst_grow = ref [] in
  let func graph v =
    lst_grow:= (G.pred graph v):: !lst_grow
  in
  VS.iter (func graph) pred_set;
  List.concat (!lst_grow);;

(* below - actully predecc set according to dataflow direction  *)
let successor_set graph pred_set=
  v_list_to_set (successor_list graph pred_set);;
(*  *)
let remove_vot_vert vert_set vo_set=
  let func vo_set v=	
    VS.for_all (fun vo-> (G.V.label vo)<>(G.V.label v)) vo_set
  in
  VS.filter (func vo_set) vert_set;;

(* TAIL-RECURSIVE - manually checked *)
let rec hop_k_times k graph v_set vo_set=
  if (k>1) then
    let v_set= remove_vot_vert (successor_set graph v_set) vo_set in
    hop_k_times (k-1) graph v_set vo_set
  else
    remove_vot_vert (successor_set graph v_set) vo_set;;

(* TAIL-RECURSIVE  - manually checked*)
let rec voter_introFST k graph v_set vo_set=
  let places_arrived = hop_k_times k graph v_set vo_set in
  if ((VS.cardinal places_arrived)>0) then  
    let new_vo_set= (VS.union vo_set places_arrived) in
    voter_introFST  k graph places_arrived new_vo_set
  else
      VS.union vo_set places_arrived;;


(*READY BUT VERY NON-optimal- non decreasing*)
let find_Gfst filename k_hops=
  (*let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let vert_names=find_out_vert_names gate_rec_list pr_outs_list in
  let graph= build_graph gate_rec_list in*)
  
  let graphSstruct= build_graph_MEM filename in
  let vert_names=List.map (fun v-> G.V.label v) (VS.elements (snd (snd graphSstruct))) in
  let graph= fst graphSstruct in


  let all_v= find_all_vertex graph in	
  let out_vers=find_out_vert_list all_v vert_names in
  let result = voter_introFST k_hops graph (v_list_to_set out_vers) (v_list_to_set out_vers) in
  (result , (VS.cardinal result));;


(* DEBUGGING FUNCTION *)
let names_to_be_protected k graph v_set vo_set=
List.map G.V.label (VS.elements (voter_introFST k graph v_set vo_set));;
(* END DEBUGGING FUNCTION *)





(*
let rec mark_dist graph urgency_list gate_rec_list k=
    let func v urgency_list gate_rec_list k=
	let further_step= (G.pred graph v) in
	marking v gate_rec_list k;
	List.append  urgency_list further_step
    in
    let rec_call graph urgency_list gate_rec_list=
      let new_urgency_list = List.filter (fun ver-> ver<>(List.hd  urgency_list)) urgency_list in
	mark_dist graph new_urgency_list gate_rec_list
    in
      if (List.length urgency_list>0) then
	let new_list=func (List.hd  urgency_list) urgency_list gate_rec_list k in
	rec_call graph new_list gate_rec_list;;
  
(*  *)
let rec more_equal graph k urgency_list gate_rec_list=
  let v_less_k k v= ((G.Mark.get v) < k)  in
  let v_equal_k k v=  ((G.Mark.get v) = k)  in
  let to_protect = (List.filter (v_equal_k k) (find_all_vertex graph) ) in

    G.Mark.clear graph;
    mark_dist graph urgency_list gate_rec_list k;
    List.iter (G.remove_vertex graph) (List.filter (v_less_k k) (find_all_vertex graph) );
    if ((G.nb_vertex graph)>0) then
      List.append to_protect (more_equal graph k to_protect gate_rec_list)
    else 
      to_protect;;*)

