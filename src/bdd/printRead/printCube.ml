open Cudd
(* open Cudd.Bdd *)
open Format
open TimingGl
open MethChoice

let autTransCube (str:string) =
    let ch1 = String.sub str 0 1 in
      match ch1 with
	| "+" -> "1"
	| "!" -> "0"
	| "D" -> "B"
	| _ -> infO#com "[ERR]autTransCube";exit 1

;;

(* Printing BDD and conversions  *)
(*translate level #1*)
let printCubeElem (elt)  =
      match elt with
	| Man.False -> "!"
	| Man.True -> "+"
	| Man.Top ->  "D"
;;

(* translate bdd to string list of +!D interm form *)
let cub2strL  ?limit:(limit=100) bdd =
  let printBdd bdd =
    let cubeStrL= ref [] in
    let cubeStr=ref "" in
    
    if (Bdd.is_true bdd) then  
	cubeStrL:=["~true"]
    else 
      if (Bdd.is_false bdd) then 
	 cubeStrL:=["~false"]
      else  
	Bdd.iter_cube (*all cubes of BDD*)
	  (
		fun cube ->
		begin
		  cubeStr:=""; (*start new str*)
		  Array.iteri (*through cube*)
			(
			  fun i elt ->  
		      let newLetter=(printCubeElem elt) in
	cubeStr := (!cubeStr)^newLetter
			)
		    cube;
		    cubeStrL:=!cubeStrL @ [!cubeStr]
		end
	  )
	bdd;
	!cubeStrL
  in
    printBdd bdd;;

(*Bdd.iter_cube (*all cubes of BDD*)
	  (
		fun cube ->		
		  Array.iteri (*through cube*)
			(
			  fun i elt ->  
		      print_string (printCubeElem elt) 
			)	
		    cube
	  )
	bdd;;

Bdd.iter_cube (*all cubes of BDD*)
	  (
		fun cube ->
		  begin
		  print_string "1\n";
		  print_string (string_of_int (Array.length cube));
		  let arr= Array.map (fun elt-> printCubeElem elt) cube  in
		  print_string (String.concat "" (Array.to_list arr))
		  end
	  )
	bdd;;


 print : (Format.formatter -> int -> unit) ->
       Format.formatter -> 'a t -> unit
Bdd.print (Format.print) 
*)
