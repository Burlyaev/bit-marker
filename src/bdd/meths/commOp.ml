open Cudd
open StrucAlloc

let ( $+ ) (bdd1:v_T) (bdd2:v_T) = (Bdd.dor bdd1 bdd2);;
let ( $* ) (bdd1:v_T) (bdd2:v_T) = (Bdd.dand bdd1 bdd2);;
let  dN  (bdd1:v_T)  = (Bdd.dnot bdd1 );;
