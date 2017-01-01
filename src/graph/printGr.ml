open Graph
open Path
open ItcBEN
open CirG

(* exporting to .dot files to look*)

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

(*exporting graph structure to see it as a drawing*)
let export_graph graph filename_graph= (*function exports a graph to the file*)
   let file = open_out_bin filename_graph in
   Dot.output_graph file graph;;


(*exports 2 graph - sequential and conbinatorial- of the circuit*)
(*let expGrStr (grStr:grL_T) =
  export_graph grStr.combI fCombGr.gr;
  export_graph grStr.seq fSegGr.gr;;


*)
