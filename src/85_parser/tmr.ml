(*TMR transformation related*)
(*TMR trasformation*)
      (*
      let tmr_ed = tmr_trans filename;;
      let pr_inps_list= inputs_for_n_circuit filename 1 "inputs";;
      let pr_outs_list= inputs_for_n_circuit filename 1 "outputs";;
      let voters_outs=voter_ins_outs pr_outs_list  pr_inps_list (last_name_num tmr_ed);; (* voters at the outputs *)
      let new_full_secription_recs= List.concat [tmr_ed;List.concat voters_outs];;
      list_to_file filenameTMR_out (List.map str_to_eq new_full_secription_recs);; (* writing the TMR-ed programme to the file *)
      export_graph (build_graph new_full_secription_recs) filenameTMR_graph;;
      *)
      (*TODO:  renaming during split - think about that-> RE-check*)
(* TMR Transformation *)
let tmr_trans filename=
  let programm = no_internal_dff (pure_circuit_sub filename) in
  let pr_inps_list= inputs_for_n_circuit filename 1 "inputs" in
  let pr_outs_list= inputs_for_n_circuit filename 1 "outputs" in
  let ex_list=pr_inps_list in
  let m_part= mem_part programm in
  let c_part= comb_part programm in
  let fin_prog_part_1 = comb_union c_part ex_list in(* compinational circuit is copied after this step *)
  let fin_prog_part_2 = mem_union m_part ex_list in(* sequential circuit with disconnected memory inouts - before the voter insertion procedure *)
  (* voter insertion procedure *)
  let temp= List.hd c_part in
  let last_name_num= gname_num temp.name in
(*   let mem_cell_numb= List.length m_part in *)
  let v_part =voter_ins m_part  ex_list last_name_num in (* voter list in records list form *)
List.concat [fin_prog_part_1; fin_prog_part_2; List.concat v_part];;

let last_name_num tmr_ed=
  let name_str_list= List.map(fun record-> record.name) tmr_ed in
  let last_name=List.nth name_str_list ((List.length name_str_list)-1) in
gname_num last_name;;