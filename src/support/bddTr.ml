open Cudd

let oneOther aL bL= 
List.concat 
      (List.map2 ( fun p v -> [p;v]) aL bL) 
;;

let xorFL prL funL = 
  List.map2 
	  (fun primeV func-> Bdd.nxor primeV func) 
  prL funL 
;;