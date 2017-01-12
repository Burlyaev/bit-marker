(* library that transforms the read combinational description to sequential graph*)
open Parser_itcBEN
open Graph
open Graph_build
open CombTOseq2
open Ckt_to_l2hdl
open IoLST


(*takes a gate_rec list and name- and tries to find gate_rec inside this list*)
(*[ERR] redifinition - fanOutIn*)
  let gate_rec_of_name gate_rec_list v_name=
    List.find (fun gate_rec-> gate_rec.name=v_name ) gate_rec_list;;

(* deletes combinatorial circuit from a graph *)
(*[ERR] redifinition - fanOutIn*)
  let del_comb graph_ref gate_rec_list ver=
      let v_name= G.V.label ver in
      let gate_rec= gate_rec_of_name gate_rec_list v_name in
      let if_mem= (gate_rec.op="DFFX") in
      let pred_lst= G.pred !graph_ref ver in
      let succ_lst= G.succ !graph_ref ver in
      
      if (not if_mem) then 
	begin
(* 	  print_string ("\ndel_comb::"^(G.V.label ver)^"\n"); *)
	  let pred_W_suc graph_ref succ_lst pred_ver=
	    List.iter (fun succ_ver-> G.add_edge !graph_ref pred_ver succ_ver) succ_lst
	  in
	  List.iter (pred_W_suc graph_ref succ_lst) pred_lst;
	  G.remove_vertex !graph_ref ver;
	end;;
(*  *)

(*retruns vertexes that are in the graph and in grate_rec lst*)
  let rec_to_ver graph gate_rec_lst =
    let ver_lst= ref [] in (*empty vertices lst*)

    let one_ver gate_rec_lst ver= (*if vertex name is in the gate_rec list then add to outputs*)
      if (List.exists (fun gate_rec-> gate_rec.name = (G.V.label ver)) gate_rec_lst) then
      (*if *)
	ver_lst:= ver :: !ver_lst
    in
    G.iter_vertex (one_ver gate_rec_lst) graph; (*iterate though all vertices*)
    !ver_lst;;
(*  *)
(*creating the purely sequentiial graph with input and outs vertex returning*)
  let build_graph_MEM filename=
      let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in (*create gate_rec lst*)
      let pr_inps_list= inputs_for_n_circuit filename 1 "inputs" in  (*input from file*)
      let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in (*outs from netlist file*)
      let graph= build_graph gate_rec_list in (*build graph  from gate_rec lst*)
      let graph_ref = ref graph in (*reference (copy) file*)
      let spec_signals=["CLOCK"] in (*special signals*)
      let inp_vS= ref VS.empty in  (*empty input vertex set*)
      let out_vS= ref VS.empty in (*empty output vertex set*)

      inp_vS:= v_list_to_set (rec_to_ver !graph_ref (inp_gate_name_list spec_signals gate_rec_list pr_inps_list));
      out_vS:= v_list_to_set (rec_to_ver !graph_ref (out_gate_name_list spec_signals gate_rec_list pr_outs_list));

      let check_ver graph_ref inp_vS out_vS ver =(*let ver = List.nth all_v 0;;*)
	  (*print_string ((G.V.label ver)^"\n");*) 
	if (VS.for_all (fun out-> (G.V.label out)<>(G.V.label ver)) !out_vS) then 
	  begin
	    del_comb graph_ref gate_rec_list ver;(*deleting the vertex if cobinational*)
	  end;
      in
    G.iter_vertex (check_ver graph_ref inp_vS out_vS) !graph_ref;
    (!graph_ref, (!inp_vS,!out_vS));;

(**** DEBUGGING FUNCTIONS *****)
(*

let gr_copy= G.copy graph;;
let a=  del_comb_IO  !graph  pr_inps_list pr_outs_list;;
let grA= fst a;;
let inpS= VS.iter (fun v-> print_string((G.V.label v)^"\n")) (fst (snd a));;
let outS= VS.iter (fun v-> print_string((G.V.label v)^"\n")) (snd (snd a));;

G.iter_vertex (fun v-> print_string((G.V.label v)^"\n")) grA;;

*)