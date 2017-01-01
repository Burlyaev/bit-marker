open Cudd
open StrucAlloc
open Contamin
(* [TODO] should I delete inputs? before checking inclusion  
In reality inputs are not sere anyway- but I can delete outputs- not necessary
*)

(* define most popular conditions:
    (cond [accSS; stSp; newSS])
 *)
(* we reached fixed point *)
let condFixP lstSS=
  let accSS= List.hd lstSS in
  let newSS= List.nth lstSS 2 in
  Bdd.is_included_in (newSS) (accSS);;

(* nxt stSp= prev stSp *)
let condSelfP lstSS=
    let oldSS= List.nth lstSS 1 in
    let newSS= List.nth lstSS 2 in
  Bdd.is_included_in (newSS) (oldSS);;

(*we reached no errors state space- returns true*)
let noErrors cirBdd lstSS=
    let newSS= List.nth lstSS 2 in
    let errL= errMemFind cirBdd newSS in
    (List.length errL)=0;;