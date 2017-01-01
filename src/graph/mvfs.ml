(* ELEMETNATRY NON-OVERLAPPING CYCLES  *)
open ItcBEN
open Graph
open CirG
open TimingGl
open Lst
open File



(* save to the file in the format acceptable by the external C++ program *)
let exp_MFV (grStr:grL_T) file_MVS=

  let graph= grStr.seq.gr in (*just pure graph from graph info*)
  let all_v= grStr.seq.vGL in	(*all vertices of the graph*)
  
  let enumL = enumF (List.length all_v) in
  let assoc_v= List.map2 (fun i v -> (v, i )) enumL all_v in (*zipping vertex whih number*)
  let ref_string_lst = List.map2 (*gather num+name to 1 str appending*)
			  (fun i v -> ((string_of_int i)^"::"^(G.V.label v)^"\n")) 
			enumL all_v 
  in
  let string_lst = (*list of successors for each vertex in order 0..(n-1)*)
		  List.map 
		    (fun src -> 
			    let succ= G.succ graph src in
			    List.fold_left 
				  (fun str suc -> 
					(str^(string_of_int (List.assoc suc assoc_v ) )^" ")
				   ) 
			    "" succ
		    )
		  all_v 
  in
  list_to_file (file_MVS^".ref.txt") ref_string_lst;
  list_to_file file_MVS ([(string_of_int (List.length all_v))] @ string_lst);
  assoc_v;;

(* reading MVS from exe output file *)
let inp_MSVres assoc_v file_inp=
    let str_lst= read_file file_inp in (*read from input file*)
    let v_nums=List.map (int_of_string) str_lst in (*each file line is converted to integer*)
    let vo_lst= List.map (*from numbers we read and zipped pairs get the vertex-cutting point*)
	  (fun v_num->fst (List.nth assoc_v v_num))
    v_nums in 
    List.iter (fun vo -> infO#com ((G.V.label vo)^";")) vo_lst; (*points/DFFs -> comments*)
    vo_lst;;


type mvfs_T=
    {
      mvfsL : G.V.t list;
      size : int;
    };;

(* main procedure to get MVS *)
let minVS filename (grStr:grL_T)=
  let file_MVS =  curDir^fMVFSi in (*name of adajancy table built based on the graph structure*)
  let file_MVSout =  curDir^fMVFSo in (*output file of .exe with MVS numbers*)
  

  let assoc_v=	exp_MFV grStr file_MVS in (*(vertex, nuber) pairs *)
 
  infO#tim "\nMFVS EXECUTION ...\n"; 
  Sys.chdir (curDir^"/MVS_input/"); (*change directory*)
  ignore (Sys.command "./MVS.out");(*EXE*)
  Sys.chdir curDir; (*directory back*)
  ignore(Sys.command ("cp "^file_MVSout^" "^ifCalculFile));(*COPY*)

 
  infO#tim " MVS is found. Complete";
  let  mvs= inp_MSVres assoc_v ifCalculFile in(*reading MVS*)
  {
    mvfsL= mvs;
    size= List.length mvs
    }
  ;;
