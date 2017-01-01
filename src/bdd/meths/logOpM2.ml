(* Methodology #2 -> z,Z, o, O *)
open Cudd
open Format
open CommOp
open StrucAlloc
open CirG
open TimingGl

let m2AND (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let xc=List.nth x.vL 0 in
  let xv=List.nth x.vL 1 in

  let yc=List.nth y.vL 0  in
  let yv=List.nth y.vL 1  in

  {vname= vertL.nameV;
   vPL =[];
   vL= 
    [(xc $* yc) $+ (xc $* (dN xv))  $+ (yc $* (dN yv)) $+ (((dN xv) $* yv) $* (dN yc)) $+ (((dN yv) $* xv) $* (dN xc)); 
(*see Quine_McCluskey_opt folder*)
      xv $* yv ];
frVPL =[]; frVL =[]; vRole=None
  };;

let m2OR (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let xc=List.nth x.vL 0 in
  let xv=List.nth x.vL 1 in

  let yc=List.nth y.vL 0  in
  let yv=List.nth y.vL 1  in
  {vname= vertL.nameV;
    vPL =[];
    vL= 
    [ (xc $* yc) $+ (xc $* xv) $+ (yc $* yv) $+ ((dN xc) $* ((dN xv) $* yv)) $+ ((dN yc)$*((dN yv) $* xv));
(*see Quine_McCluskey_opt folder*)
    xv $+ yv];
    frVPL =[]; frVL =[]; vRole=None
  };;

let m2NOT (vertL:vertLab) (x:vBDD_T) =
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in
  {vname= vertL.nameV;
    vPL =[];
   vL=[ x0; dN x1];
   frVPL =[]; frVL =[]; vRole=None
  };;


let m2DFFX (vertL:vertLab) (x:vBDD_T) =
 let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in
  {vname= vertL.nameV;
     vPL =[];
   vL= [x0;x1];
  frVPL =[]; frVL =[]; vRole=None
  };;

(* Voter and Error function *)

let m2Vot (inV:vBDD_T )  =
   let g1=List.nth inV.vL 0 in
   let g2=List.nth inV.vL 1 in 
{
  vname=inV.vname;
  vPL=[]; (*we just introduced voter- it's not base variable anymore*)
  vL=[Bdd.ite (g1) (g1) (Bdd.dnot g1) ; 
	  Bdd.ite (g1) (g2) (Bdd.dnot g2)];
  frVPL=inV.frVPL;
  frVL=inV.frVL;
  vRole=inV.vRole
};;

(* Translation function *)

let transM2 str =
  let ch1 = String.sub str 0 1 in
  let ch2 = String.sub str 1  1 in
  match (ch1, ch2) with
	| ("+","+") -> "1"  (*one *)		
	| ("!","!") -> "0*" (*corrupted zero*)
	| ("+","!") -> "0"  (*zero*)
	| ("!","+") -> "1*" (*corrupted one*)
	| ("D","+") -> "1c*" (*correct and corrupted one*)
	| ("D","!") -> "0c*" (*correct and corrupted zero *)
	| ("D","D") -> "any" (*any of possible options*)
	| ("+","D") -> "cV"  (* correct value: zero or one *)
	| ("!","D") -> "V*"  (* corrupted value: corrupted zero or corrupted one*)
	| _ -> infO#com "[ERR]transM2";exit 1
  ;;

let transRevM2 (str:string) =
  match str with
	|  "1" -> "++" 
	|  "0"->  "+!"
	|  "T"->  "+D"
	| _ -> infO#com "[ERR]transRevM2";exit 1
  ;;
(*	|  "o" -> "++" 
	|  "Z" -> "!!"
	|  "z"->  "+!"
	|  "O" -> "!+"
	|  "-"->  "+D"
	|  "~" -> "!D" (*[TODO] think about that - orig.it was - -*)
	| _ -> infO#com "[ERR]transRevM2";exit 1
  ;;*)

(* Error corruption *)
  (*corruption in precise domaine*)
let errInjM2 (varBdd:vBDD_T) stSp=
  let b1=List.nth varBdd.vL 0 in
  let b2=List.nth varBdd.vL 1 in 
  let vf1=Bdd.dnot b1 in
  let vf2=Bdd.dnot b2 in 
  let nf1= List.hd (Bdd.list_of_support b1) in
  let nf2= List.hd (Bdd.list_of_support b2) in
  let ss1=Bdd.compose  nf1 vf1 stSp in
  Bdd.compose  nf2 vf2 ss1 ;;

(*existance of err in varBdd in stSp-> returns bool*)
(* there is error-> true; no error-> false  *)
let checkErrM2 (varBdd:vBDD_T) stSp= 
   let b1=List.nth varBdd.vL 0 in
  (*let b2=List.nth varBdd.vL 1 in  *)
   let vf1=Bdd.dnot b1 in
  (*let vf2=Bdd.dnot b2 in *)
   let combM2 = vf1 in 
   not (Bdd.is_inter_empty combM2 stSp);;