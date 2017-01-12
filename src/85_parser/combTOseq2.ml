open Parser_itcBEN
open Graph
open Graph_build


(*set of graph vertices of tune G.*)
module VS = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = G.vertex 
  end );;

(* TAIL-RECURSIVE NOW - changed(re-written)*)
(*translation from list to set, [?] of graph vertices*)
let v_list_to_set out_vers= 
  let rec list_to_set out_vers accumulator=
    match (List.length out_vers) with
      |0-> accumulator
      |1-> VS.union (VS.singleton (List.hd out_vers)) accumulator
      |_->   
	  let accum = VS.union (VS.singleton (List.hd out_vers)) accumulator in
	      list_to_set (List.tl out_vers) accum;
  in
list_to_set out_vers VS.empty;;
  
(*  

let rec v_list_to_set out_vers=
  if ((List.length out_vers) > 1) then
    VS.union (VS.singleton (List.hd out_vers))( v_list_to_set (List.tl out_vers))
  else
    if ((List.length out_vers) = 1) then
      VS.singleton (List.hd out_vers)
    else
      VS.empty;;*)

				(*let inp_wire_name_list= pr_inps_list;;
					let spec_signals=["CLOCK"];;*)
(*?? how this function is used  ??*)
let inp_gate_name_list spec_signals gate_rec_list pr_inps_list = 

  let fun1 spec_signals gate_rec=
      let gate_inps= gate_rec.ins in (*inputs of a gate*)
  (*     let g_inps_set= v_list_to_set gate_inps in *)
      let g_pure_inps= (*inps without spec signals*)
	      (List.filter (*if it's not a spec signal*)
		  (fun inp->  (List.for_all (fun sign->sign<>inp) spec_signals)) gate_inps) in 
      let fun2 name = (* if name exists among pure inputs if the gate*)
	List.exists (fun gate_one_ins -> name=gate_one_ins) g_pure_inps
      in
    List.exists fun2  pr_inps_list (*if there is in pr_inps_list a name that is 
primary input for this gate excluding the special wires*)
  in
List.filter  (fun1 spec_signals) gate_rec_list;; (*filter gate-rec lst by creteria of spec signals*)

(* let inp_g_recs= inp_gate_name_list spec_signals gate_rec_list pr_inps_list;;(* TEST *) *)
(*??? how it is used ?? *)
let out_gate_name_list spec_signals gate_rec_list pr_outs_list = 
  let fun1_out spec_signals gate_rec=
      let gate_outs= [gate_rec.out] in
  (*     let g_inps_set= v_list_to_set gate_inps in *)
      let g_pure_outs= (List.filter (fun out->  (List.for_all (fun sign->sign<>out) spec_signals)) gate_outs) in
      let fun2 name =
	List.exists (fun gate_one_out-> name=gate_one_out) g_pure_outs
      in
    List.exists fun2  pr_outs_list 
  in
List.filter  (fun1_out spec_signals) gate_rec_list;;


(*let out_g_recs= out_gate_name_list spec_signals gate_rec_list pr_outs_list;;(* TEST *) *)
(* [NEW_L]
let inp_v_list all_v inp_g_recs = 
  let inp_g_names = List.map ( fun g_rec->g_rec.name) inp_g_recs in
  List.filter (fun ver -> (List.exists (fun name-> name= (G.V.label ver)) inp_g_names)) all_v;;

let vnameL_with_sigL gate_rec_list signal_lst =
  let for_one_sig gate_rec_list sig_name=
    let g_rec_lst= List.filter (fun g_rec-> List.exists (fun wire_name-> wire_name = sig_name) (List.append g_rec.ins [g_rec.out]) ) gate_rec_list in
    List.map (fun g_rec-> g_rec.name) g_rec_lst
  in
  List.concat(List.map (for_one_sig gate_rec_list) signal_lst);;
*)

(*
let v_IO_lst graph gate_rec_list seek_str =
  let pr_IO_list= inputs_for_n_circuit filename 1 seek_str in
  let pr_IO_lst_wo_CL = List.filter () pr_IO_list in
  vnameL_with_sigL gate_rec_list pr_IO_list ;;*)