(*FILE-NAMES*)

let name="s"^"27";;
(* TXT files*)
let filename	=	curDir^"/inputs/"^name^".ckt";;
let filename_out=	curDir^"/outputs/"^name^".l2hd.txt";; (*circuit discription in L-HDL*)
let filenameTMR_out=	curDir^"/outputs/"^name^"_tmr.l2hd.txt";;(* TMR circuit discr in L-HDL*)
(* GRAPH files *)
let filename_graph= 	curDir^"/outputs/graphs/"^name^".dot";; (*graph in .dot*)
let filename_graph_MEM= 	curDir^"/outputs/graphs/"^name^".MEM.dot";; (*graph in .dot*)
let filenameTMR_graph= 	curDir^"/outputs/graphs/"^name^"_tmr.dot";;(*TMR graph in .dot *)
(* let filenameTC_graph= 	curDir^"/outputs/graphs/"^name^"_tc.dot";; *)

(* FILE-list for iteration *)
  let file_dirL= Array.to_list (Sys.readdir (curDir^("/inputs/")));;
  let names_unsorted=List.filter (fun name -> String.contains name '.')file_dirL ;;
(* let cir_n= Array.of_list (List.sort (str_sort) names_unsorted);; *)
let cir_n=  names_unsorted;;
let cir_f= List.map (fun name-> curDir^"/inputs/"^name) cir_n;;

let ass_fn= List.map2 (fun c_f c_n-> (c_f, c_n)) cir_f cir_n;;
let all_cir_files = file_sortA (Array.of_list cir_f);;
let all_cir_names= Array.mapi (fun i cir_file-> List.assoc cir_file ass_fn) all_cir_files;;

let all_grALL_files= Array.map (fun name-> curDir^"/outputs/graphs/"^name^".dot") all_cir_names;;
let all_grSEQ_files= Array.map (fun name-> curDir^"/outputs/graphs/"^name^".SEQ.dot") all_cir_names;;
let all_INF_files= Array.map (fun name-> curDir^"/outputs/stat/inf/"^name^".INF.txt") all_cir_names;;

let time_file alg_name i= 
      curDir^"/outputs/stat/time/"^alg_name^"/"^(all_cir_names.(i))^"."^alg_name^".TIME.txt";;
let vo_file alg_name i= 
      curDir^"/outputs/stat/vo_num/"^alg_name^"/"^(all_cir_names.(i))^"."^alg_name^".VO.txt";;



let fFile start_file file_to_process = Array.to_list (Array.sub all_cir_files start_file file_to_process);;
let fGrFile start_file file_to_process = 

Array.to_list (Array.sub all_cir_files start_file file_to_process);;

(* let file_list_s= 		List.map (fun name-> curDir^"/inputs/s"^name^".ckt") file_list;; *)
let filename_stat= 	curDir^"/outputs/stat/statistics.txt";;
let filename_time= 	curDir^"/outputs/stat/stat_time.txt";;
(* supporting files for Minimum Feedback Vertex Set procedure *)
let file_MVS = curDir^"/MVS_input/MVSinput.txt";;
let file_MVSout = curDir^"/MVS_input/MVS.txt";;