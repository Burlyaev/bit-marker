(* Comparison Function*)

let paths_len p1 p2=
  (List.length p2)-(List.length p1);;

let sort_paths win_Tlst = 
  List.sort (fun p1 p2-> (List.length p2)-(List.length p1)) win_Tlst;;

let sort_int int_lst= List.sort (fun i1 i2 -> i2-i1) int_lst;;
(*  *)
let compar_fun tup1 tup2=
     (snd tup2) -  (snd tup1);;



let longest_p n_list=
    (* if there are more than 1 candidate then we keep the longest, possible several*)
    match  (List.length n_list) with 
    |0->
(* 	print_string "Program Error longest_p"; *)
	0
    |_-> 
	let sorted_lst=List.sort paths_len n_list in
	List.length (List.hd sorted_lst)(* List.filter (fun path-> (List.length (List.hd sorted_lst))=(List.length path)) sorted_lst *)
    ;;

  let str_sort a b =
	let numCKTa=String.sub a 1 ((String.length a)-1)  in
	let numCKTb=String.sub b 1 ((String.length b)-1)  in
	let numA=String.sub  numCKTa 0 ((String.length numCKTa) -5)  in
	let numB=String.sub numCKTb 0 ((String.length numCKTb) -5)  in
	let intA=
	  try int_of_string(numA) 
	  with | Failure _ -> print_string ("\nFAIL:"^numA^"::"^numCKTa ^"!!!\n"); 0
	in
	let intB=
	try int_of_string(numB) 
	  with | Failure _ -> print_string ("\nFAIL:"^numB^"::"^numCKTb^"!!!\n"); 0
	in
	if intA<intB then -1 else
	if intA>intB then 1 else
	0;; 

print_string (string_of_int (str_sort "s27.ckt" "s298.ckt"));;

let fmem_compare pair1 pair2=
  let mem1= snd pair1 in
  let mem2= snd pair2 in
  mem1-mem2;;
