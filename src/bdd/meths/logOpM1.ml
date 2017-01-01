(* Methodology #1 -> 0,1, T, B *)
open Cudd
open Format
open CommOp
open StrucAlloc
open CirG
open TimingGl


let m1AND (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in

  let y0=List.nth y.vL 0  in
  let y1=List.nth y.vL 1  in

  {vname= vertL.nameV;
   vPL =[];
   vL= 
    [x0 $* y0;
    ((dN x0) $* x1) $+ (x0 $* y1) $+ (( dN y0) $* y1) $+ (y0 $* x1)];
   frVPL =[]; frVL =[]; vRole=None
  };;

let m1OR (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in

  let y0=List.nth y.vL 0  in
  let y1=List.nth y.vL 1  in
  {vname= vertL.nameV;
    vPL =[];
    vL= [ ((dN x1)$*x0)$+ (x0$*y1)$+ ((dN y1)$*y0)$+(y0 $*x1); (*correct see interm rep McK*)
      x1 $* y1 (*correct*)];
    frVPL =[]; frVL =[]; vRole=None
  };;

let m1NOT (vertL:vertLab) (x:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in
   {vname= vertL.nameV;
    vPL =[];
    vL= [ x1;
	    x0 ];
     frVPL =[]; frVL =[]; vRole=None
  };;

let m1DFFX (vertL:vertLab) (x:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in
  {vname= vertL.nameV;
    vPL =[];
    vL= [ x0;
	  x1 ];
    frVPL =[]; frVL =[]; vRole=None
  };;

(* Voter and Error function *)

let m1Vot (inV:vBDD_T )  =
   let g1=List.nth inV.vL 0 in
   let g2=List.nth inV.vL 1 in 
{
  vname=inV.vname;
  vPL=[]; (*we just introduced voter- it's not base variable anymore*)
  vL=[Bdd.ite (Bdd.dor g1 g2) (g1) (Bdd.dnot g1) ; 
	  Bdd.ite (Bdd.dor g1 g2) (g2) (Bdd.dnot g2)];
  frVPL=inV.frVPL;
  frVL=inV.frVL;
  vRole=inV.vRole
};;

(* Translation function *)

let transM1 str =
  let ch1 = String.sub str 0 1 in
  let ch2 = String.sub str 1  1 in
  match (ch1, ch2) with
	  | ("+","+") -> "T"
	  | ("!","!") -> "B"
	  | ("+","!") -> "1"
	  | ("!","+") -> "0"
	  | ("D","+") -> "T|0"
	  | ("D","!") -> "B|1"
	  | ("D","D") -> "any"
	  | ("+","D") -> "T|1"
	  | ("!","D") -> "B|0"
	  | _ -> infO#com "[ERR]transRevM1";exit 1
  ;;


let transRevM1 str =
  match str with
	  | "T" -> "++"
	  | "B" -> "!!"
	  | "1" -> "+!"
	  | "0" -> "!+" 
	  | _ -> infO#com "[ERR]transRevM1";exit 1
  ;;

(* Error corruption *)
    (*corruption in TB domqine*)
let errInjM1 (varBdd:vBDD_T) stSp=
   let b1=List.nth varBdd.vL 0 in
   let b2=List.nth varBdd.vL 1 in 
   let vf1=Bdd.dnot b1 in
   let vf2=Bdd.dnot b2 in 
   let freeIniSS1 = Bdd.exist b1 stSp in
   let freeIniSS2 = Bdd.exist b2 freeIniSS1 in
   Bdd.dand (Bdd.dand vf1 vf2 ) freeIniSS2;;


(*existance of err in varBdd in stSp-> returns bool*)
(* there is error-> true; no error-> false  *)
let checkErrM1 (varBdd:vBDD_T) stSp= 
  let b1=List.nth varBdd.vL 0 in
  let b2=List.nth varBdd.vL 1 in 
  let vf1=Bdd.dnot b1 in
  let vf2=Bdd.dnot b2 in 
  let combM1 = Bdd.dand vf1 vf2 in
  not (Bdd.is_inter_empty combM1 stSp);; (*if intersection empty*)
				    (*empty- non error  *)
