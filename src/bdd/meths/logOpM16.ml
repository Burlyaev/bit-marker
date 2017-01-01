(* Methodology #3 -> 16 value logic *)
open Cudd
open Format
open CommOp
open StrucAlloc
open CirG
open TimingGl


let m3AND (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let a1=List.nth x.vL 0 in
  let b1=List.nth x.vL 1 in
  let c1=List.nth x.vL 2 in
  let d1=List.nth x.vL 3 in

  let a2=List.nth y.vL 0 in
  let b2=List.nth y.vL 1 in
  let c2=List.nth y.vL 2 in
  let d2=List.nth y.vL 3 in
  {vname= vertL.nameV;
   vPL =[];
   vL= 
    [(a1 $+ a2) $+ ((c1$*d2) $+ (d1$*c2)); 
     b1 $* b2;
  (b1$*c2)$+ ((c1$*b2) $+ (c1$*c2));
  (b1$*d2) $+ ((d1$*b2) $+ (d1$*d2))
  ];
frVPL =[]; frVL =[]; vRole=None
  };;

let m3OR (vertL:vertLab) (x:vBDD_T) (y:vBDD_T)=
  let a1=List.nth x.vL 0 in
  let b1=List.nth x.vL 1 in
  let c1=List.nth x.vL 2 in
  let d1=List.nth x.vL 3 in

  let a2=List.nth y.vL 0 in
  let b2=List.nth y.vL 1 in
  let c2=List.nth y.vL 2 in
  let d2=List.nth y.vL 3 in

  {vname= vertL.nameV;
    vPL =[];
    vL= 
    [  a1 $* a2;
       (b1 $+ b2) $+ ((c1$*d2) $+ (d1 $* c2));
	(a1$*c2)$+ ((c1$*a2) $+ (c1$*c2));
	(a1$*d2) $+ ((d1$*a2) $+ (d1$*d2))
    ];
    frVPL =[]; frVL =[]; vRole=None
  };;

let m3NOT (vertL:vertLab) (x:vBDD_T) =
  let a1=List.nth x.vL 0 in
  let a2=List.nth x.vL 1 in
  let a3=List.nth x.vL 2 in
  let a4=List.nth x.vL 3 in

  {vname= vertL.nameV;
    vPL =[];
   vL=[ a2;a1;a4;a3];
   frVPL =[]; frVL =[]; vRole=None
  };;


let m3DFFX (vertL:vertLab) (x:vBDD_T) =
  let a1=List.nth x.vL 0 in
  let a2=List.nth x.vL 1 in
  let a3=List.nth x.vL 2 in
  let a4=List.nth x.vL 3 in
  {vname= vertL.nameV;
     vPL =[];
   vL= [a1;a2;a3;a4];
  frVPL =[]; frVL =[]; vRole=None
  };;

(* Voter and Error function *)

let m3Vot (inV:vBDD_T )  =
  let a=List.nth inV.vL 0 in
  let b=List.nth inV.vL 1 in
  let c=List.nth inV.vL 2 in
  let d=List.nth inV.vL 3 in

{
  vname=inV.vname;
  vPL=[]; (*we just introduced voter- it's not base variable anymore*)
  vL=[
      Bdd.ite (Bdd.dnot d) a d;(*[TODO]: re-check these equentions*) 
      Bdd.ite (Bdd.dnot c) b c;
      Bdd.ite (c) (Bdd.dnot c) (c) ; 
      Bdd.ite (d) (Bdd.dnot d) (d)
      ];
  frVPL=inV.frVPL;
  frVL=inV.frVL;
  vRole=inV.vRole
};;

(* Translation function *)

let transM3 str =
  let ch1 = String.sub str 0  1 in
  let ch2 = String.sub str 1  1 in
  let ch3 = String.sub str 2  1 in
  let ch4 = String.sub str 3  1 in

  let tr1 = match ch1 with  (*if there is zero in the set*)
	    | "+"-> "0"
	    | "!"-> ""
	    | "D"-> "0?"
	    | _ -> infO#com "[ERR]transM3";exit 1
  in
  let tr2 = match ch2 with  (*if there is one in the set*)
	    | "+"-> "1"
	    | "!"-> ""
	    | "D"-> "1?"
	    | _ -> infO#com "[ERR]transM3";exit 1
  in
  let tr3 = match ch3 with   (*if there is error-zero in the set*)
	    | "+"-> "Z"
	    | "!"-> ""
	    | "D"-> "Z?"
	    | _ -> infO#com "[ERR]transM3";exit 1
  in
  let tr4 = match ch4 with (*if there is error-one in the set*)
	    | "+"-> "O"
	    | "!"-> ""
	    | "D"-> "O?"
	    | _ -> infO#com "[ERR]transM3";exit 1
  in
  
  let t1t2=
	match (tr1, tr2) with
	|("0","1") -> "T"
	|("0?","1?") -> "T?"
	|(_,_) -> tr1^tr2
  in
  
  let t2t3=
	match (tr3, tr4) with
	|("Z","O") -> "B"
	|("Z?","O?") -> "B?"
	|(_,_) -> tr3^tr4
  in
  
  t1t2^t2t3;;


let transRevM3 (str:string) =
  match str with
	|"1" -> "!+!!" 
	|"0" -> "+!!!"
	|"T" -> "++!!"    
	|"B" -> "++++"
	| _ -> infO#com "[ERR]transRevM3";exit 1
  ;;

(* Error corruption *)
  (*corruption in 16-values domaine- SEU*)
let errInjM3 (inV:vBDD_T) iniStSp=
  let a=List.nth inV.vL 0 in
  let b=List.nth inV.vL 1 in
  let c=List.nth inV.vL 2 in
  let d=List.nth inV.vL 3 in

  let had_0 = Bdd.dand iniStSp a in
  let had_1 = Bdd.dand iniStSp b in
  let had_z = Bdd.dand iniStSp c in
  let had_o = Bdd.dand iniStSp d in

  let notHad_0= Bdd.dand (Bdd.dnot a) (Bdd.exist a had_0) in
  let notHad_1= Bdd.dand (Bdd.dnot b) (Bdd.exist b had_1) in  
  let notHad_z= Bdd.dand (Bdd.dnot c) (Bdd.exist c had_z) in
  let notHad_o= Bdd.dand (Bdd.dnot d) (Bdd.exist d had_o) in
  
  let no_0_but_o= Bdd.dand (d) (Bdd.exist d notHad_0) in
  let no_1_but_z= Bdd.dand (c) (Bdd.exist c notHad_1) in  
  (* for the precision we return not correct vals to correct in the case of SEU *)
  
  let no_z_but_1= Bdd.dand (b) (Bdd.exist b notHad_z) in
  let no_o_but_0= Bdd.dand (a) (Bdd.exist a notHad_o) in

  List.fold_left Bdd.dor (no_0_but_o) [no_1_but_z;no_z_but_1;no_o_but_0];;

(*existance of err in varBdd in stSp-> returns bool*)
(* there is error-> true; no error-> false  *)
let checkErrM3 (inV:vBDD_T) stSp= 
   let vf3=List.nth inV.vL 2 in
   let vf4=List.nth inV.vL 3 in
   let combM1 = Bdd.dor vf3 vf4 in
   let intersect= Bdd.dand combM1 stSp in
   not (Bdd.is_false intersect );;