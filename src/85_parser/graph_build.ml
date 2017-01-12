open Graph
open Path
open Parser_itcBEN

module Gate_rec = struct 
  type t = string
  type label = string
end;;

module Int = struct 
  type t = int 
  let compare = compare 
  let hash = Hashtbl.hash 
  let equal = (=)
  let default = 0
end;;

(*  *)
module G = Imperative.Digraph.AbstractLabeled(Gate_rec)(Int);;
(* module G = Pack.Digraph(Gate_rec)(Int);; *)
(*  *)


(*extracts from gate list - the list of names and create a node for each gate with the same name*)
let vertex_list  gate_inf_list=
    let func_add gate_rec =
      G.V.create (gate_rec.name)
    in
List.map func_add gate_inf_list;;
(*  *)
module Dot = Graph.Graphviz.Dot(struct
   include G (* use the graph module from above *)
   let edge_attributes _ = [] 
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = [`Shape `Box]
   let vertex_name v = G.V.label v
   let default_vertex_attributes _ = []
   let graph_attributes _ = []
end);;

(* build graph from list of gate records *)
let build_graph gate_rec_list =
    let graph= G.create() in (*empty graph*)
    let v_list= vertex_list gate_rec_list in (*nodes vreation*)

    let create_edges gate_rec_list graph= (*func creates edges between nodes*)
	  let signal_takers rec_s graph rec_t=
	    if List.exists (fun v -> rec_s.out=v) rec_t.ins then (*if out of res_s exists in res_t.ins list *)
	      let s= List.find (fun v-> G.V.label v=rec_s.name) v_list in (*find res_s in vertex list*)
	      let t= List.find (fun v-> G.V.label v=rec_t.name) v_list in (*fins res_t in vertex list*)
(* 	      let edge_temp *)
	      G.add_edge_e graph (G.E.create s 1 t) (*connect them*)
(* 	      G.add_edge graph s t *)
	  in
	let signal_source rec_s=
	  List.iter (signal_takers rec_s graph) gate_rec_list (*apply to to all gates: 
		      signal_tankes res_s graph res_t*)
	in
      List.iter signal_source gate_rec_list (*apply fun to all gates in the struct list*)
    in
List.iter (G.add_vertex graph) v_list; (*additing vertices to a graph*)
create_edges gate_rec_list graph; (*adding edges to a graph*)
graph;; (*return graph*)

			  (*let graph= G.create();;
			  let v_list= vertex_list gate_rec_list;;
			  List.iter (G.add_vertex graph) v_list;;
			  let create_edges gate_rec_list graph=
				let signal_takers rec_s graph rec_t=
				  if List.exists (fun v -> rec_s.out=v) rec_t.ins then 
				  let s= List.find (fun v-> G.V.label v=rec_s.name) v_list in
				  let t= List.find (fun v-> G.V.label v=rec_t.name) v_list in
				  G.add_edge graph s t
				in
			      let signal_source rec_s=
				List.iter (signal_takers rec_s graph) gate_rec_list 
			      in
			    List.iter signal_source gate_rec_list;;
			  create_edges gate_rec_list graph;;*)

			  (* module D = Graph.Traverse.Dfs(G);; *)
			  (*let file = open_out_bin filename_graph ;;
			  let () = Dot.output_graph file graph;;*)

(*  *)
let export_graph graph filename_graph= (*function exports a graph to the file*)
   let file = open_out_bin filename_graph in
   Dot.output_graph file graph;;



(**** Additional available algorithms and function I looked at ****)
(*
module W = struct 
  type label = G.E.label
  type t = int
  let weight x = x
  let zero = 0
  let add = (+)
  let compare = compare
end;;

module Dij = Path.Dijkstra(G)(W) 

let v1=List.nth all_v 1;;(* TEST *)
let v2=List.nth all_v 2;;(* TEST *)
let (p,l) = Dij.shortest_path graph v1 v2;;(* TEST *)

module Bel = Path.BellmanFord(G)(W);;

let v1= List.nth all_v 0;;
let v2= List.nth all_v 1;;
let v3= List.nth all_v 9;;

let diat_ht=Bel.all_shortest_paths graph v1;;
Bel.H.find diat_ht v2;;

Bel.H.inter diat_ht v3;;

(*module Op= Oper.I(G);;
let temp= Op.transitive_closure ~reflexive:false graph;;
export_graph temp filenameTC_graph;;*)*)

(* EOF *)





















(****************************************************************)
      (*let v = G.V.create ("first_v");;
      G.add_vertex graph v;;
      let v1 = G.V.create ("second_v");;
      G.add_vertex graph v1;;*)
(*let create_edges gate_rec_list graph=
      let signal_takers rec_s graph rec_t=
	if List.exists (fun v -> rec_s.out=v) rec_t.ins then 
	let s= List.find (fun v-> G.V.label v=rec_s.name) v_list in
	let t= List.find (fun v-> G.V.label v=rec_t.name) v_list in
	G.add_edge graph s t
      in
    let signal_source rec_s=
      List.iter (signal_takers rec_s graph) gate_rec_list 
    in
  List.iter signal_source gate_rec_list;;*)



  (*let v = G.V.create ("first_v");;

      G.add_vertex graph v;;
      let v1 = G.V.create ("second_v");;
      G.add_vertex graph v1;;*)
