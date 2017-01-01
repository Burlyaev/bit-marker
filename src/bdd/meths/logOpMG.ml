(* Methodology #1 -> 0,1, T, B *)
open Cudd
open Format
open CommOp
open StrucAlloc
open CirG
open TimingGl


let m3AND (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in

  let y0=List.nth y.vL 0  in
  let y1=List.nth y.vL 1  in

  {vname= vertL.nameV;
   vPL =[];
   vL= 
    [x0 $* y0;
     x1 $* y1];
   frVPL =[]; frVL =[]; vRole=None
  };;

let m3OR (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in

  let y0=List.nth y.vL 0  in
  let y1=List.nth y.vL 1  in
  {vname= vertL.nameV;
    vPL =[];
    vL= [ x0 $+ y0;
          x1 $+ y1];
    frVPL =[]; frVL =[]; vRole=None
  };;

let m3NOT (vertL:vertLab) (x:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in
   {vname= vertL.nameV;
    vPL =[];
    vL= [  dN x0;
	   dN x1 ];
     frVPL =[]; frVL =[]; vRole=None
  };;

let m3DFFX (vertL:vertLab) (x:vBDD_T)=
  let x0=List.nth x.vL 0 in
  let x1=List.nth x.vL 1 in
  {vname= vertL.nameV;
    vPL =[];
    vL= [ x0;
	  x1 ];
    frVPL =[]; frVL =[]; vRole=None
  };;

(* Voter and Error function *)

let m3Vot (inV:vBDD_T )  =
   let g1=List.nth inV.vL 0 in
   let g2=List.nth inV.vL 1 in 
{
  vname=inV.vname;
  vPL=[]; (*we just introduced voter- it's not base variable anymore*)
  vL=[g2 ; 
      g2];
  frVPL=inV.frVPL;
  frVL=inV.frVL;
  vRole=inV.vRole
};;

(* Translation function *)

let transM3 str =
  let ch1 = String.sub str 0 1 in
  let ch2 = String.sub str 1  1 in
  match (ch1, ch2) with
	  | ("+","+") -> "1"
	  | ("!","!") -> "0"
	  | ("+","!") -> "1*"
	  | ("!","+") -> "0*"
	  | ("D","+") -> "+0"
	  | ("D","!") -> "+1"
	  | ("D","D") -> "D"
	  | ("+","D") -> "-0"
	  | ("!","D") -> "-1"
	  | _ -> infO#com "[ERR]transRevM1";exit 1
  ;;


let transRevM3 str =
  match str with
	  | "T" -> "DD"
	  | "1" -> "++"
	  | "0" -> "!!" 
	  | _ -> infO#com "[ERR]transRevM1";exit 1
  ;;

(* Error corruption *)
    (*corruption in TB domqine*)
let errInjM3 (varBdd:vBDD_T) stSp=
   let b1=List.nth varBdd.vL 0 in
   let vf1=Bdd.dnot b1 in
   let freeIniSS1 = Bdd.exist b1 stSp in
   Bdd.dand vf1 freeIniSS1;;


(*existance of err in varBdd in stSp-> returns bool*)
(* there is error-> true; no error-> false  *)
let checkErrM3 (varBdd:vBDD_T) stSp= 
  let b1=List.nth varBdd.vL 0 in
  let b2=List.nth varBdd.vL 1 in 
  let vf1=Bdd.dnot b1 in
  let vf2=Bdd.dnot b2 in 
  let combM1 = Bdd.dand vf1 vf2 in
  not (Bdd.is_inter_empty combM1 stSp);; (*if intersection empty*)
				    (*empty- non error  *)
