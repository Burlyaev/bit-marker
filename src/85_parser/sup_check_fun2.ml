(* LONGEST PATH without cycle overlap *)

let one_hop graph nonC_v st_lst=
    let one_start nonC_v path =
      let p_end = List.hd (List.rev path) in   
      let new_ends=G.pred graph p_end in
      if ((List.length new_ends)>0) then
	List.map (fun new_end-> 
	    if (VS.subset (VS.singleton  new_end) nonC_v) then 
	      (List.append (path) [new_end],true)  
	    else 
	      (List.append (path) [new_end],false))
	new_ends
      else 
	[(path,false)]
    in
  List.concat (List.map (one_start nonC_v)   st_lst);;
  
(*   	if (VS.exists (fun ver-> (G.V.label ver) = (G.V.label new_end)) nonC_v) then *)

let clean_both_cyc nonC_v new_tul_list =
let func nonC_v tup=
  let path = fst tup in
  if ((List.length path) =2) then 
    let beg= List.nth path 0 in
    let ending= List.nth path 1 in
    if ((VS.subset (VS.singleton  beg) nonC_v)||(VS.subset (VS.singleton  ending) nonC_v)) then
      true
    else 
      false
  else
    true
in
List.filter (func nonC_v) new_tul_list;;

let choose_winner graph nonC_v ver=
  let condition= ref true in
  let st_lst= ref[] in
  let new_tul_list= ref[] in
  let cand_tup=ref [] in

  st_lst:=[[ver]];
  condition:=true;
  while (!condition) do
    new_tul_list:= one_hop graph nonC_v !st_lst;
    cand_tup := List.filter (fun tup-> (snd tup)=true) !new_tul_list;
    if ((List.length !cand_tup)=0) then 
	condition:=false
    else 
       st_lst := List.map (fun tup-> fst tup) !cand_tup;
  done;
  new_tul_list:=clean_both_cyc nonC_v !new_tul_list;
  st_lst := List.map (fun tup-> fst tup) !new_tul_list;
  longest_p !st_lst;;
  
(*   
let st_lst=[[List.nth all_v 0]];;
let st_lst=one_hop graph nonC_v st_lst;;
nvl (fst ( List.nth st_lst 0) );;
let path = List.hd !st_lst;;

choose_winner (List.nt all_v 0);;     

let ver= List.nth all_v 0;;
choose_winner nonC_v ver;;
*)

let find_cycFREE_path filename=
  let graph= fst (build_graph_MEM filename )in
  let all_v=find_all_vertex graph in
  (* 1.find all cyclic pathses *)
  let el_loops_lst= el_cyc_G graph in
  (* 2. merge to the set all vertecis in cycles *)
  let loop_v_set= v_list_to_set( List.concat el_loops_lst) in
  (* 3.choose  any vertex in graph but not in the set#2*)
  let all_set= v_list_to_set all_v in
  let nonC_v = VS.diff all_set loop_v_set in
  let win_Tlst = ref[] in
  (* 4. for #3 build all possible longest pathes which do not include #2 *)

  (* 5. find the winner for #4 and comrare it with the Global winner *)
  (* 6.repeat 3-6 for other vertex until there is no more vetices anymore that will not belong to #2 *)
  win_Tlst:=[];
  for i=0 to ((List.length all_v)-1) do
    win_Tlst:=List.append (!win_Tlst) [choose_winner graph nonC_v (List.nth all_v i)];
(*   print_string (string_of_int i) *)
  done;
(*   let win_Tlst= List.map (fun ver->  choose_winner nonC_v ver) all_v in *)
  let sorted_winners= sort_int !win_Tlst in
(*   List.iter(fun ver-> print_string ("\t"^(G.V.label ver)^"::"^(string_of_int(choose_winner nonC_v ver))^"\n"))all_v; *)
  List.hd sorted_winners;;


