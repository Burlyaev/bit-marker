(* ELEMETNATRY NON-OVERLAPPING CYCLES  *)
open Parser_itcBEN
open Graph
open Graph_build
open CombTOseq2
open CombTOseq
open Ckt_to_l2hdl
open FanOutIn
(* save to the file in the format acceptable by the external C++ program *)
let exp_MFV filename file_MVS=

  let graphSstruct= build_graph_MEM filename in (*building graph ifor from circ*)
  let graph= fst graphSstruct in (*just pure graph from graph info*)

  let all_v= find_all_vertex graph in	(*all vertices of the graph*)
  let string_lst= ref [] in
  let ref_string_lst= ref [] in
  let temp_str = ref "" in
  let assoc_v= ref [] in
  assoc_v:=[];

  for i=0 to ((List.length all_v)-1) do (*we go through all vertices*)
    assoc_v:= List.append (!assoc_v) [(List.nth all_v i,i)]; (*zipping vertex whih number*)
    let ref_str = ((string_of_int i)^"::"^(G.V.label (List.nth all_v i))^"\n") in (*number + vertex name as 1 string*)
    ref_string_lst:= List.append (!ref_string_lst) [ref_str]; (*gather num+name to 1 str appending*)
  done;

  string_lst:= List.append (!string_lst) [(string_of_int (List.length all_v))];
  for i=0 to ((List.length all_v)-1) do
(*       print_string ("i="^(string_of_int(i))^"\n");(*DEBUGGING*) *)
(*       print_string ("j="^(G.V.label(List.nth all_v i))^"\n");(*DEBUGGING*)	 *)
    let src= List.nth all_v i in
    let succ = G.succ graph src in
    temp_str:=""; 
    if ((List.length succ)>0) then begin
      for j=0 to ((List.length succ)-1) do
(* 	  print_string ("\tj="^(string_of_int(j))^"\n");(*DEBUGGING*)	 *)
(* 	  print_string ("\t"^((G.V.label (List.nth succ j) ) )^"\n"); (*DEBUGGING*)	 *)
	temp_str:=((!temp_str)^(string_of_int (List.assoc (List.nth succ j) !assoc_v ) )^" ");
      done;
    end;
    string_lst:= List.append (!string_lst) [(!temp_str)];
  done;
(*   List.iter print_string (!string_lst); *)
(*   List.iter print_string (!ref_string_lst); *)
  list_to_file (file_MVS^".ref.txt") !ref_string_lst;
  list_to_file file_MVS !string_lst;
  !assoc_v;;
(* reading MVS from exe output file *)
let inp_MSVres assoc_v file_inp=
    let str_lst= read_file file_inp in (*read from input file*)
    let v_nums=List.map (int_of_string) str_lst in (*each file lien is converted to integer*)
    let vo_lst= List.map (*from numbers we read and zipped pairs get the vertex-cutting point*)
	  (fun v_num->fst (List.nth assoc_v v_num))
    v_nums in 
    List.iter (fun vo -> print_string ((G.V.label vo)^";")) vo_lst; (*just print the names of cutting points/DFFs*)
    vo_lst;;
(* main procedure to get MVS *)
let minVS filename=
  let file_MVS = curDir^"/MVS_input/MVSinput.txt" in (*name of adajancy table built based on the graph structure*)
  let file_MVSout = curDir^"/MVS_input/MVS.txt" in (*output file of .exe with MVS numbers*)
  let assoc_v=exp_MFV filename file_MVS in (*(vertex, nuber) pairs *)
  let ifCalculFile= curDir^"/MVS_calculated/"^filename in
  
(* check if the result already been calculated *)
  let tryRead=  
      try read_file ifCalculFile
      with _ -> (print_string "\nMVFS NOT FOUND \n";["*"])
  in
  let readFile = 
      if ((List.length tryRead)=1) then
	  if (List.hd tryRead ="*") then 
	      "none"
	  else ifCalculFile
      else ifCalculFile
  in
  
  let exit_code =
      match readFile with 
      |"none"-> (print_string "\nMFVS EXECUTION ...\n"; 
		  Sys.chdir (curDir^"/MVS_input/"); (*change directory*)
		  ignore (Sys.command "./MVS.out");(*EXE*)
		  Sys.chdir curDir; (*directory back*)
		  ignore(Sys.command ("cp "^file_MVSout^" "^ifCalculFile));(*COPY*)
		   0
		    )
      | _-> 0
  in
  
  if (exit_code=0) then 
    begin
      print_string" MVS is found. Complete";
      let  mvs= inp_MSVres assoc_v ifCalculFile in(*reading MVS*)
      (mvs, List.length mvs)
    end
  else 
    begin
      print_string"[ERR]:: minVS ";
      ([], (-1))
    end;;



  

(*
  
let tabInt_line arr =
 let line= ref "" in
 for i=0 to ((Array.length arr)-1) do
  line:= (!line)^"\t"^(string_of_int arr.(i))
  done;
 (!line)^"\n";;
  
  *)
