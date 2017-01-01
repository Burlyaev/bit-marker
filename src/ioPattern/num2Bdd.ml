open Cudd
open IoType
open StrucAlloc
open Lst
open TimingGl
open SsManip


(*retruns list of aut state num and corresp Bdd*)  
let num2BddMap man (sBddL: vBDD_T list) (aut: ioType_T list) =
  let maxStN= maxStA aut in (*max number of the state in aut*)
  let stIntL = enumF (maxStN+1) in (*we also have 0-initial state*)
  let encodLen =  (*length of bin represente of the biggest state number*)
	      let a=int2binNum maxStN  in 
	      a.binLen in
  let binStL = List.map (*convert intergers to bin format of the same length*)
		(fun stInt-> 
			let binForm= int2binNum stInt in 
			wideBinStr binForm.binForm encodLen) 
		stIntL 
  in
  let str2bdd str= (*fun form bdd representation of autom state accord str*)
      let strLen= String.length str in
	List.fold_left (
		fun acc num ->
		  let bit= String.get str num in
		  let bdd=List.nth sBddL num in
		  let sV= List.hd bdd.vL in (*autom is encoded with 1 bit per varibale*)
		  let bddBitF= 
			match bit with 
			| '0' -> (fun x-> Bdd.dnot x)
			| '1' -> (fun x-> x)
			| _  -> infO#com "[ERR]resAutoSF:: str2bdd";exit 1
		  in
		  Bdd.dand acc (bddBitF sV)
		) 
	(Bdd.dtrue man ) (enumF strLen)
  in
  let bddStL = List.map str2bdd binStL in (*bdd representation of states 0..maxStN of autom*)
  List.map2 (fun num bdd -> (num, bdd)) stIntL bddStL;;


(* 
List.map (fun p -> _print (snd p)) num2BddM;;


 *)