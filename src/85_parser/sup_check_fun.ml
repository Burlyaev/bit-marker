(* Date: 06.03.1989
Status: Complete, but re-check when possible W1;
 *)
(* LONGEST ACYCLIC PATH *)
let if_all_end path_type_list inp_v_lst =
(List.length path_type_list)=0;;


let label_lists labLL =
  let funcLL labL =
    let funcL lab= 
	print_string ((G.V.label lab)^"->")
    in
    List.iter funcL labL;
    print_string "\n\n"
  in
List.iter funcLL  labLL;;




let long_acyc_path filename  =
  let graph= fst (build_graph_MEM filename) in (*COMP part eliminated!!!*)
  let all_v= find_all_vertex graph in
  let path_type_list= ref [] in
  let new_path_type_list = ref [] in
  let p_tL_loops = ref [] in
  let pL_to_cont = ref [] in
  let plT_stopped = ref [] in
  let p_stopped= ref [] in
  let fst_cand= ref [] in
  let vo_set= VS.empty in
  let endCond= ref true in
  let winLEN= ref 0 in
  let win_p= ref [] in

(*   print_string ("\n"^filename^"\n"); *)
  winLEN:=0;
  p_stopped:=[];
  for i=0 to ((List.length all_v)-1) do
    path_type_list:= [{p=[List.nth all_v i]; code =2}];
		    
    endCond:= false;
    while (not !endCond) do
(* 	print_string ("Suspecios\n");	 *)
      new_path_type_list:= hop_1_time_paths_list_from_pathes graph !path_type_list vo_set;
(* 	  print_string ("NOT Sucpecios\n"); *)
(*       label_lists (List.map (fun path_type-> path_type.p) !p_tL_loops); (*DEBUG*) *)
(*     print_string ("1\n");	 *)
      p_tL_loops:= loops_find_after_1_hop !new_path_type_list;
(*      print_string ("2\n");	 *)
(*       label_lists (List.map (fun path_type-> path_type.p) !p_tL_loops); (*DEBUG*) *)
(*      List.iter (fun p_t-> print_string (string_of_int (p_t.code))) !p_tL_loops;
      print_string "\n";*)
(* print_string ("4\n"); *)

      pL_to_cont:= List.filter (fun p_t-> p_t.code = 2 ) !p_tL_loops;
      plT_stopped:= List.filter (fun p_t-> p_t.code <>2) !p_tL_loops;
(* 	  print_string ("5\n"); *)
      path_type_list:= !pL_to_cont;
      endCond:= ((List.length !pL_to_cont)=0); 
(* 	  print_string ("pL_to_cont::"^(string_of_int i)^("~"^(G.V.label (List.nth all_v i))^"~")^"\t::"^(string_of_int(List.length !pL_to_cont))^"\n"); *)
(* print_string ("6\n"); *)
      p_stopped:=sort_paths (List.map (fun path_type-> path_type.p) !plT_stopped);
	if ((List.length !p_stopped)=0) then 
	  fst_cand:=[] 
	else 
	  fst_cand:= List.hd !p_stopped;
      
(*     print_string ("2.1\n"); *)
(*       label_lists  (!p_stopped); (*DEBUGGING*) *)
(*       print_string ("Longest->"^(string_of_int(List.length !fst_cand))^"\n"); *)
(* print_string ("7\n"); *)
      if ((List.length !fst_cand) > !winLEN) then 
	begin
	  win_p:= !fst_cand; 
	  winLEN:= List.length !win_p
	end;
(*     print_string ("8\n"); *)
      done;
  done;
( !win_p,!winLEN);;


let show_paths path_type_list=
  let func1 path_type=
    List.map (fun v-> G.V.label v)path_type.p
  in
  List.map func1 path_type_list;;

(*

show_paths !path_type_list;;
show_paths !cand_paths;;

*)

let lApath_show filename=
  let res= long_acyc_path filename in
  let path_type= fst res in
  (nvl path_type,snd res);;

(* Re-check when possible *)
