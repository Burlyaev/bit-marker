(*Mixed voter-insertion algorithm*)

let k_hops=1;;
(* let find_MIXed filename k_hops= *)

let graphSstruct= build_graph_MEM filename ;;
let graph= fst graphSstruct ;;

(* 1) find cycles *)
let min_V_struct= minVS curDir filename;;
let min_vo_lst= fst min_V_struct;; 
(* 2) introduce voters to the output of 1) *)
let vo_list= ref [];;
vo_list:= List.append !vo_list min_vo_lst;;
(* 3) put voters at the outputs *)

let out_vS= (snd (snd graphSstruct));;

vo_list:=List.append !vo_list (VS.elements out_vS);;
(*
 List.map (fun ver-> G.V.label ver) !vo_list ;;
*)

(* 4) start hopping from all intoduced voters and introduce more voters if necessary based on the simple my algorithm for voter intro *)
let vo_set = v_list_to_set !vo_list;;
let ini_path_type_list= List.map (fun v->{p=[v]; code =2}) 				(VS.elements vo_set);;
let result=voter_intro_STRG k_hops graph ini_path_type_list vo_set;;



