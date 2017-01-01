open Cudd
open Cudd.Bdd
open Format
open TimingGl
open File
open Lst
open StrucAlloc
open PrintCube
open MethChoice

(*give me the list of variables and the list of strings(output of sub2strL) I will return translated list of translated values *)
let varPrintF  (cirBdd: cirBdd_T) (strL:string list) =
      let varL= cirBdd.insVar@cirBdd.memVar@cirBdd.statVar@cirBdd.outVar in
      let strProc str=
	let memValL= List.map 
		      (fun bdd -> 
			String.concat ""
			      (List.map 
				  (fun pos->
(* 				      try *)
					String.sub str pos 1
				     (*with _ -> print_string ("[ERR]varPrintF::"^(string_of_int (String.length str))^"::");exit 1
				    *)
				  ) 
				(bdd.vPL)
			      ) 
		      )
		    varL 
	in
	let transValL= List.map2 
	      (fun memVal bdd-> match bdd.vRole with
				|AutB -> autTransCube memVal (*if its aut bits-> it's binary encoding always*)
				|_->transCube memVal
	      ) 
	    memValL varL in
	let nameValP= List.map2 
			(fun bdd v-> (bdd.vname,v)) 
		      varL transValL in
	nameValP
      in
(*     List.map(fun str -> infO#tim str) strL ; *)
    let namValPLL=List.map (strProc) strL in
    namValPLL;;


(* printing cubes of bdd to file *)
let prBdd2file (cirBdd: cirBdd_T) bdd =
  let strL= cub2strL  bdd in
  let pairL= varPrintF cirBdd strL in
  let file= !fDataR  in
  let linesL= read_file file in (*read old file; to add or ex*)
  (* form string lines   *)
  let form maxSp str = 
      let len = String.length str in
      str^(String.make (maxSp- len) ' ')
  in
  let signalNum= List.length (List.hd pairL) in
	  (*   let combinNum= List.length pairL in *)
  let formLine num= 
	let valL= List.map (fun pL-> List.nth pL num ) pairL in
	let subStr= List.map (fun p-> form 5 (snd p)) valL in 	
	let oneStr= String.concat "" subStr in
	oneStr
  in
  let valStrL= List.map formLine (enumF signalNum) in
  let valStr2L = List.map (fun str-> (str^"  |")) valStrL in

 
  (* if file is empty or not*)
  let ifFileEmp= (List.length linesL)<3 in 
  (* but if empty we need also names    *)
  let namesL = List.map (fun p-> fst p) (List.hd pairL) in
  let withNames= List.map2 (fun name valStr-> (form 20 name)^valStr) namesL valStr2L in 
  (*add to old files    *)

  match ifFileEmp with 
  |true -> list_to_file file withNames
  |false->  
	  let valStr3L = List.map   (*if file is not empty add*)
		 (fun num -> 
	      (List.nth linesL num)^(List.nth  valStr2L num))  
		  (enumF (List.length valStr2L)) in
	list_to_file file valStr3L
  ;;

(* printing separator to data file *)
let prSepar2file sep =
 let file= !fDataR  in 
 let linesL= read_file file in (*read old file; to add or ex*)
 let linNum= List.length linesL in
 let sepNum=String.length sep in 
 let sepL= List.map 
		  (fun pos -> 
			match ((sepNum-1)-pos)<0 with
			|true -> " "
			|false-> String.sub sep pos 1
		  ) 
	      (enumF linNum) in

 let sepL= List.map2 (fun lin s-> lin^"¦"^s^"¦") linesL sepL in
 list_to_file file sepL;;


(* if the global paramers allows printing cube to file - it does *)
let bdd2file (cirBdd: cirBdd_T) bdd=
  match collP.intern_data with 
  |true -> prBdd2file cirBdd bdd 
  |false -> ignore 0
;;

(*  printing separator sep vertically to the latest opened data file for priting cubes - but it prints if global intern_data is true  *)
let dFileV sep =
    match collP.intern_data with 
      |true -> prSepar2file sep
      |false -> ignore 0
    ;;
  