(* MODIF: 14.03.2013- "rec" deleted since it's not recursive function *)
let str_build_UNI apply_func step_k k filename=
  let startF= ref 0.0 in
  let finishF= ref 0.0 in
  let res_temp = ref[] in
  let count = ref 0 in
  let vo_num_str = ref "" in
  let times_str = ref "" in

  startF:= Unix.gettimeofday() ;
  res_temp:=[apply_func filename 1];
  finishF:= Unix.gettimeofday() ;
    let output_str= string_of_int (snd(List.hd !res_temp)) in
  vo_num_str:=((!vo_num_str)^output_str^"\t");
      let timeDIFF_str= Printf.sprintf "%f" (!finishF -. !startF ) in
  times_str:=((!times_str)^timeDIFF_str^"\t");
  count:=step_k;
  while !count< k do
(*     print_string ("Iterration for k="^(string_of_int (!count))^"\n"); (*SUP/Debug*) *)
    startF:= Unix.gettimeofday() ;
    res_temp:=[apply_func filename !count];
    finishF:= Unix.gettimeofday() ;
      let output_str= string_of_int (snd(List.hd !res_temp)) in
    vo_num_str:=((!vo_num_str)^output_str^"\t");
      let timeDIFF_str= Printf.sprintf "%f" (!finishF -. !startF ) in
    times_str:=((!times_str)^timeDIFF_str^"\t");
    count:=!count+ step_k
  done;
[!vo_num_str; !times_str];;

let begin_end_UNI apply_func step_k k filename =
    let result= str_build_UNI apply_func step_k k filename in
    [ (* filename^"::\t" ^*)(List.nth result 0)^"\n";(List.nth result 1)^"\n"];;

let through_files_UNI apply_func step_k k file_list_s filename_stat filename_time =
  let lst_str=List.map (begin_end_UNI apply_func step_k k) file_list_s in
  let vo_num = List.map (fun el-> List.nth el 0) lst_str in
  let time_num = List.map (fun el-> List.nth el 1) lst_str in
  list_to_file filename_stat vo_num;
  list_to_file filename_time time_num;;


