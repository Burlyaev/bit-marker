let file_mem_sort fileA=
  let tuples fileA =
    let num_mem_one_file filename=
      let gate_rec_list=no_internal_dff (pure_circuit_sub filename) in
      let gateMem= List.filter(fun g_rec-> g_rec.op="DFFX") gate_rec_list in
      List.length gateMem
    in
    let tupleA= Array.map (fun filen-> (filen, num_mem_one_file filen)) fileA in
  Array.to_list tupleA
  in
  let sorted = List.sort (fmem_compare) (tuples fileA) in
  sorted;;

let read_5inf_lines filename = 
		let lines = ref [] in
		let chan = open_in filename in
	
	  for i=0 to 3 do
		lines := input_line chan :: !lines
	  done; 
	  close_in chan;
	  List.rev !lines ;; 


let ext_num line=
let num_need a b c = b in    
  Scanf.sscanf line "\t%s\t# %i %s " num_need;;

let get_extention filename =
  let name_separated= Str.split (Str.regexp "[.]+") filename in
  List.hd (List.rev name_separated);;

let benchITC_inf filename= 
  let chan = open_in filename in
  let intA=ref [||] in
  let line= ref "" in
  let reg_ins_num=Str.regexp "# \\(.*\\) \\(inputs\|input\\)" in
  let reg_outss_num=Str.regexp "# \\(.*\\) \\(outputs\|output\\)" in
  let reg_dff_num=Str.regexp "# \\(.*\\) D-type flipflops" in
  let reg_gates_num=Str.regexp "# \\(.*\\) gates" in
  let regL=[reg_ins_num;reg_outss_num;reg_dff_num;reg_gates_num] in
  let apply_reg intA line reg=
    if (Str.string_match  reg line 0) then 
      intA:= Array.append !intA [|int_of_string(Str.matched_group 1 line)|]
  in
  for i=0 to 20 do
    line:=input_line chan;
    List.iter (apply_reg intA !line )regL;
  done;
  !intA;;

 
let file_inf filename=	
  let ckt_dff filename=
    let five_lines= read_5inf_lines filename in
    let nums= List.map (ext_num) five_lines in
    Array.of_list nums
  in
(* print_string ("Attended::"^filename); *)
  match (get_extention filename) with
  |"ckt"-> ckt_dff filename
  |"bench"-> benchITC_inf filename
  | _ -> ckt_dff filename (*TODO:: change this by Risign the Failure*)	
;;

let file_sortA fileA=
  let files_infs= Array.map file_inf fileA in
  let pairL= ref[] in
  let sorted = ref[] in
  for i=0 to ((Array.length fileA) -1) do
    pairL:=List.append !pairL [(fileA.(i), Array.get (files_infs.(i)) 2)] (*2- num DFFX*)
  done;
  sorted := List.sort fmem_compare !pairL;
  Array.of_list(List.map (fun pair-> fst pair) !sorted);;





