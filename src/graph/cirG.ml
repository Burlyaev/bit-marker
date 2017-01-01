open Graph
open Path
open ItcBEN

(* Graph Type definition +*)
module Vert_T = struct
  type t = string
end;;

module Edg_T = struct 
  type t = int 
  let compare = compare 
  let default = 0
end;;

module G = Imperative.Digraph.AbstractLabeled(Vert_T)(Edg_T);;
(* Graph Type definition -*)

type gRole= Inp | Out | Gate;;

type vertLab=
    {
      nameV: string;
      roleV: gRole;
      opV: string;
      insV: string list;
      outV: string
    }

type libGr =
   {	
	file  : string;  (*name of netlist file*)
	inpVL : vertLab list; (*names of inps*)
	outVL : vertLab list; (*names of outs*)
 	gateVL: vertLab list; (* all gates describtion*)
    }

let renamOut (outN:string)= (*renaming function for global outputs*)
  String.concat "" ["_out_"; outN] ;;

let itc2libG filename= (*important - since rename outs*)
  let cirI=benITC_gRECL filename in
  let inpV= List.map  		
      (fun name-> 
	      {nameV=name;
		roleV= Inp;
		opV="INPUT";
		insV=[];
		outV=name } )
	cirI.inpL 
  in
  let outV= List.map  		
	(fun name-> 
	      {nameV=(renamOut name);  (*renaming of outputs- now outs are separate*)
	      roleV= Out; 
	      opV="OUTPUT";
	      insV=[name];
	      outV=(renamOut name) })
	  cirI.outL 
  in
  let gateV= List.map (fun gate-> {nameV=gate.name;roleV= Gate;opV=gate.op;insV=gate.ins;outV=gate.out}) 	
	    cirI.gateL in
  {	
	file  = filename;  (*name of netlist file*)
	inpVL = inpV; (*inps labelsnames of inps*)
	outVL = outV; (*outs labels*)
 	gateVL= gateV; (* all gates vertex lables*)
   }
  ;;
  
(*extracts from gate list - the list of names and create a node for each gate with the same name*)
let vertex_list  gate_inf_list=
    let func_add gate_rec =
      G.V.create (gate_rec.nameV)
    in
List.map func_add gate_inf_list;;
(*  *)


type grType = CompGrIO | CompGrWithInps | CompGrWoInps | SeqGrWithInps | SeqGrWoInps;; (*graph type: comb/seq*)

(*graph type*)
type grI_T=
  {
    gr :G.t; (*graph structure*)
    vGL	: G.V.t list; (*vertices list of graphs*)
    gType:grType (*type of the graph: seq/comb*)
  }

(*topy for collection of netlist graphs*)
type grL_T=
  {
    combIO	:grI_T;
    combI	: grI_T;
    comb	:grI_T;
    seq		:grI_T;
    seqI	:grI_T;
  }

(* build graph from list of gate records *)
let build_graph (vertLabL:vertLab list) gType =
    let graph= G.create() in (*empty graph*)
    let v_list= vertex_list vertLabL in (*nodes vreation*)
    List.iter (G.add_vertex graph) v_list; (*additing vertices to a graph*)

    let create_edges = (*func creates edges between nodes*)
	  let signal_takers rec_s graph rec_t=
	    if (List.exists (fun v -> rec_s.outV=v) rec_t.insV) then (*if out of res_s exists in res_t.ins list *)
	      let s= List.find (fun v-> G.V.label v=rec_s.nameV) v_list in (*find res_s in vertex list*)
	      let t= List.find (fun v-> G.V.label v=rec_t.nameV) v_list in (*fins res_t in vertex list*)

	      G.add_edge_e graph (G.E.create s 1 t) (*connect them*)
	  in
	  let signal_source rec_s=
	    List.iter (signal_takers rec_s graph) vertLabL (*apply to to all gates: 
		      signal_tankes res_s graph res_t*)
	  in
      List.iter signal_source vertLabL (*apply fun to all gates in the struct list*)
    in
    create_edges; (*adding edges to a graph*)
    {
      gr=graph; 
      vGL=v_list; (*vertex list- components  of the build graph*)
      gType=gType (*type of graph: combin/seq/...*)
    }
  ;;


(*organise seq graph from combin types and generate all graph types*)

(* deletes vertex ver from a graph if it's comb circ*)
  let del_comb graph_ref (gateL:vertLab list) ver=
      let v_name= G.V.label ver in (*returns the name of the vertex*)
      let gate_rec= List.find (fun gRec -> gRec.nameV=v_name) gateL in (*finds it in records*)
      let if_mem= (gate_rec.opV= ItcBEN.memOpS) in (*if it's a memory cell*)
      let if_inp= (gate_rec.roleV=Inp) in (*if it's inputs*)
      let pred_lst= G.pred !graph_ref ver in (*predecessor*)
      let succ_lst= G.succ !graph_ref ver in (*successors*)
      
      if (not (if_mem||if_inp)) then  (*if this gate is combin gate and not a memory*) (*and not inputs*)
	begin
	  let pred_W_suc graph_ref succ_lst pred_ver=  (*fun: connect 1 pred with all succs*)
	    List.iter (fun succ_ver-> G.add_edge !graph_ref pred_ver succ_ver) succ_lst
	  in
	  List.iter (pred_W_suc graph_ref succ_lst) pred_lst; (*each pred connect to each successor*)
	  G.remove_vertex !graph_ref ver; (*remove the combin gate-vertex from graph*)
	end;;

  (*convert combin graph to seq *)
  let comb2seq graph_ref gateL =
    G.iter_vertex (fun ver -> del_comb graph_ref gateL ver) !graph_ref; 
   !graph_ref;;



let itc2Grs (libStr:libGr) =
  (*combin gr with Is and Os*)
  let grCombIO = (*type 1*)
	build_graph (libStr.inpVL @libStr.outVL @libStr.gateVL ) CompGrIO  (*gr with gates and inputs*)
  in
  (*combin gr with Is *)
  let grCombInps = (*type 1*)
	build_graph (libStr.inpVL @libStr.gateVL) CompGrWithInps  (*gr with gates and inputs*)
  in
  (*combin gr W/O Is and Os*)
  let grCombWoInps =  (*type 2*)
	build_graph (libStr.gateVL) CompGrWoInps (*gr with gates*)
  in

  (*seq graph- no inputs*)
  let grCombin = build_graph (libStr.gateVL) SeqGrWoInps in (*GR of gates *)
  let grCombinI = build_graph (libStr.inpVL @libStr.gateVL) SeqGrWoInps in (*GR of gates and inps *)
  let grSeq= comb2seq (ref grCombin.gr) (libStr.gateVL) in
  let grSeqI= comb2seq (ref grCombinI.gr) (libStr.inpVL @libStr.gateVL) in (*seq gr with inputs*)
  let vertL= G.fold_vertex (fun v l -> l@[v] ) grSeq [] in

  let grSeqWoInps= 
	{
	   gr=grSeq; 
	   vGL=vertL; (*vertex list- components  of the build graph*)
          gType=SeqGrWoInps (*type of graph: combin/seq/...*)
	}
  in
  let vertIL= G.fold_vertex (fun v l -> l@[v] ) grSeqI [] in
  let grSeqInps= 
	{
	   gr=grSeqI; 
	   vGL=vertIL; (*vertex list- components  of the build graph*)
          gType=SeqGrWithInps (*type of graph: combin/seq/...*)
	}
  in
{
  combIO =grCombIO; 
  combI =grCombInps;
  comb =grCombWoInps;
  seq = grSeqWoInps;
  seqI=grSeqInps
}
;;




