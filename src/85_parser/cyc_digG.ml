
let what_v_num ver ver_arr=
  let num= ref (-1) in
  num:=0;
  for i=0 to ((Array.length ver_arr)-1) do
    if ((G.V.label ver)=(G.V.label ver_arr.(i))) then
      num:=i
  done;
  !num;;

let what_vG1_num ver ver_arr=
  let num= ref (-1) in
  num:=0;
  for i=0 to ((Array.length ver_arr)-1) do
    if ((G1.V.label ver)=(G1.V.label ver_arr.(i))) then
      num:=i
  done;
  !num;;



let g_to_G1 graph all_Gv_arr graph_g1 all_G1v_arr =
  let v_num= Array.length all_Gv_arr in
  let succ_lst= ref[] in
  let num_d= ref 0 in
  num_d:=-1;
  for num_s=0 to (v_num-1) do
    succ_lst:= G.succ graph all_Gv_arr.(num_s);  
    for j=0 to ((List.length !succ_lst)-1) do
      num_d:= what_v_num (List.nth !succ_lst j) all_Gv_arr;
      G1.add_edge graph_g1 all_G1v_arr.(num_s) all_G1v_arr.(!num_d)
    done;
  done;;


let conv_cyc_G1_to_G  all_G1v_arr all_v ll =
    let conv_lst  all_G1v_arr all_v lst_G1=
      let conv_G1_to_G  all_G1v_arr all_v el_G1=
	let num= what_vG1_num el_G1 all_G1v_arr in
	List.nth all_v num
      in
      List.map (conv_G1_to_G all_G1v_arr all_v) lst_G1
    in
  List.map (conv_lst all_G1v_arr all_v) ll;;


let el_cyc_G graph =
  let all_v= find_all_vertex graph in
  let v_num=  List.length all_v in
  let graph_g1 = G1.create ~size:v_num () in
  let all_G1v_arr = Array.init v_num G1.V.create in
  let all_Gv_arr= Array.of_list all_v in
  let ll = ref [] in
  g_to_G1 graph all_Gv_arr graph_g1 all_G1v_arr;
  ll := find_all_cycles_johnson graph_g1;
  conv_cyc_G1_to_G  all_G1v_arr all_v !ll;;

(* DEBUGGING FUNCTION *)
let el_cyc_G_names graph=
  let cyc_lst_lst=el_cyc_G graph in
  let cyc_lst_f cyc_lst=
    List.map (fun v-> G.V.label v) cyc_lst
  in
List.map (cyc_lst_f) cyc_lst_lst;;







