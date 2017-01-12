let ren_str str1 ex_list num=
  if (not(List.exists (fun el-> el= str1) ex_list )) then
  str1^"__"^string_of_int num
  else str1;;

let no_internal_dff gate_rec_list= 
  List.filter (fun record -> (String.sub (record.name) 0 2)<>"ff") gate_rec_list;;

let mem_part gate_rec_list=
  List.filter (fun record-> record.op="DFFX") gate_rec_list;;

let comb_part gate_rec_list=
  List.filter (fun record-> record.op<>"DFFX") gate_rec_list;;


(*  *)
let ren_rec old_record ex_list num=
    let rec_name= ren_str old_record.name ex_list num in
    let rec_op	= old_record.op in
    let rec_out	=  ren_str old_record.out ex_list num in
    let rec_ins = List.map (fun input-> ren_str input ex_list num) old_record.ins in
    let  new_record = {name=(rec_name); 
			op=(rec_op);
			ins=(rec_ins);
			out=(rec_out)}
    in
    new_record;;
(*  *)
let ren_rec_lst  old_rec_lst ex_list num=
  List.map (fun record-> ren_rec record ex_list num) old_rec_lst;;

(*  *)

(*  *)
let new_gname_num old_name_num=
  old_name_num+1;;

let gname_num name_str=  
      let len= String.length name_str in
    int_of_string(String.sub name_str 2 (len-2));;

let new_Gname gname_num=
  "G_"^string_of_int(gname_num);;

let new_Wname gname_num=
  "N_newwire_"^string_of_int(gname_num);;

(*  *)

let voter3_rec_lst out_gl [in1; in2; in3] [and1_o;and2_o;and3_o] [or1_o;or2_o] g_last_num=
  let  and1 = {name= (new_Gname (g_last_num+1) ); 
			op="AND";
			ins=[in1;in2];
			out=(and1_o)} 
  in
  let  and2 = {name=(new_Gname (g_last_num+2) ); 
			op="AND";
			ins=[in2;in3];
			out=(and2_o)} 
  in
  let  and3 = {name=(new_Gname (g_last_num+3) ); 
			op="AND";
			ins=[in1;in3];
			out=(and3_o)} 
  in
  let  or1 = {name=(new_Gname (g_last_num+4) ); 
			op="OR";
			ins=[and1_o;and2_o];
			out=or1_o} 
  in
  let  or2 = {name=(new_Gname (g_last_num+5) ); 
			op="OR";
			ins=[and2_o;and3_o];
			out=or2_o} 
  in
  let  or3 = {name=(new_Gname (g_last_num+6) ); 
			op="OR";
			ins=[or1_o;or2_o];
			out=out_gl} 
  in
  let g_new_last_num =g_last_num +6 in
[and1;and2;and3;or1;or2;or3];;

(*  *)
let voter_ins m_part ex_list last_name_num= 
  let a= ref[] in
  let loop m_part ex_list last_name_num=  
    for i=0 to ((List.length m_part)-1) do
      let ith_record =List.nth m_part i in 
      let mem_inp= List.hd (ith_record.ins) in
      let rm k=ren_str mem_inp ex_list k in  
         
      for j=0 to 2 do
        let a1 v_n= "N_Voter_G_"^(string_of_int (last_name_num+6*(3*i+j)+v_n)) in
	let one_voter_rec_list=voter3_rec_lst (rm (4+j)) [rm 1; rm 2; rm 3] [a1 1;a1 2;a1 3] [a1 4;a1 5] (last_name_num+6*(3*i+j)) in
	
	a := one_voter_rec_list :: !a      
      done;
    done;
    List.rev !a;
  in
  loop m_part ex_list last_name_num;;
(*  *)
let voter_ins_outs out_parts  ex_list last_name_num= 
  let a= ref[] in
  let loop out_parts  ex_list last_name_num=
    for i=0 to ((List.length out_parts)-1) do
      let out_name =List.nth out_parts i in 
      let rm k=ren_str out_name ex_list k in  
      
      for j=0 to 0 do
	let a1 v_n= "N_Voter_G_"^(string_of_int (last_name_num+6*(3*i+j)+v_n)) in
	let rec_list=voter3_rec_lst (out_name) [rm 1; rm 2; rm 3] [a1 1;a1 2;a1 3] [a1 4;a1 5] (last_name_num+6*(3*i+j)) in

	a := rec_list :: !a	
      done;
    done;
    List.rev !a;
  in
  loop out_parts  ex_list last_name_num;;
(*  *)
let comb_union c_part ex_list = 
  let c_copy1= ren_rec_lst c_part ex_list 1 in
  let c_copy2= ren_rec_lst c_part ex_list 2 in
  let c_copy3= ren_rec_lst c_part ex_list 3 in
 List.append (List.append c_copy1 c_copy2)   c_copy3 ;;
(*  *)
let mem_union m_part ex_list =
    let mem_record_trans j record =
      let  new_rec = {name= "m"^(ren_str record.name [""] j) ; 
	  op= record.op;
	  ins= [ren_str (List.hd record.ins) ex_list (j+3);"CLOCK"];
	  out=ren_str record.out ex_list j}
      in
      new_rec;
    in
    let m_copy1=   List.map  (mem_record_trans 1)   m_part in
    let m_copy2=   List.map  (mem_record_trans 2)   m_part in
    let m_copy3=   List.map  (mem_record_trans 3)   m_part in
  List.append (List.append m_copy1 m_copy2)   m_copy3 ;;




(* HOW TO USE:: *)
(*let tmr_ed = tmr_trans filename;;
let voters_outs=voter_ins_outs pr_outs_list  pr_inps_list (last_name_num tmr_ed);; (* voters at the outputs *)
let new_full_secription_recs= List.concat [tmr_ed;List.concat voters_outs];;
list_to_file filenameTMR_out (List.map str_to_eq new_full_secription_recs);; (* writing the TMR-ed programme to the file *)
export_graph (build_graph new_full_secription_recs) filenameTMR_graph;;*)