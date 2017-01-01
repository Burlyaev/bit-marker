open Sys
(*ioAut  libs*)
open IoBits16
open IoTB
open IoPrecise
open Cudd 
open Format



(*Timing gathering*)
(* users' paramerters struct*)
type analys_T = EAA | EPA;; (*Error Accuulation Analysis - no care about outs*)
			    (*Error Propagation Analysis -  observe outputs*)
type meth_T= TB | Precise | Bits16 ;;
let meth2mult= (fun m -> match m with 
			  | TB -> 2	
			  | Precise -> 2	
			  | Bits16 -> 4
		);;
type prior_T= MvfsP| FanOutP| FanInP | MvfsOutP| SetP;;

type faultM_T= SEU|SET;;

type coll_P= 
  {
    meth: meth_T; 
    analys:analys_T; (*type of analysis: accumulation or propagation*)
    faultModel:faultM_T; (*fault-model*) 
    parK: int; (*K param- till when execute*)
    memPrior:prior_T; (*priority policy to choose nxt mem cell for voter*)
    timing: bool; (*time measurements checkpoints*)
    stats : bool; (* official measurement of size of state spaces*)
    comment :bool; (*comments about steps of execution*)
    intern_data :bool; (*values and st spaces printing*)
    reOrderFL: bool (*REORDEER FLAG- reorder during trans fun building*)
    }
(*------------------GLOBAL USER-Defined vars-----------*)
let filename= (*circuit location*) "/home/burlyaev/Dropbox/_PHD_THESIS/Simulations/erra/Build/Bench/b01_opt_r.bench";;
let ioAut = b01_Pr;; (*IO-automat*)

  (* other params	 *)
   let curDir=Sys.getcwd();; 
   let fTime= "inf/time.txt" ;;
   let fStat= "inf/stat.txt" ;;
   let fComm= "inf/Comment.txt";; 
   let fDataR= ref "inf/Data.txt" ;;
   let fDataF str= 
	fDataR:= "inf/"^str^".txt";;
    (*mvfs*)
   let fMVFSi="/MVS_input/MVSinput.txt";;
   let fMVFSo="/MVS_input/MVS.txt";;
   let fMVFScalc="/MVS_calculated/";;
   let ifCalculFile= "mvfsCalculated.tex"
    
  (*graph export location*)
   let fCombGr= "inf/combGr.dot" ;;
   let fSegGr= "inf/seqbGr.dot" ;;

(*parameters of simulation*)
  let collP= 
      {
	  meth= Precise	;
	  analys= EAA;
	  faultModel= SEU;
	  parK=30;
	  memPrior= MvfsOutP;
	  timing = true;
	  stats = true;
	  comment = true;
	  intern_data = false;
	  reOrderFL= true
      }
  ;;

(*-------------------------------------------------------------------------*)
(*class waveColl =
  object (self)
    val mutable fileN=*)


(*class for object of information collection*)
class ingCollect (param:coll_P) = 
object  (self)
   val mutable chTime= open_out fTime
   val mutable chStat= open_out fStat
   val mutable chComm = open_out fComm
(*    val mutable chData = open_out fDataR *)
   
   method tim name =
	  if param.timing then 
	    begin
	      let time =Sys.time() in (*checkpoint the system time*)
	      Printf.fprintf chTime "%s %s\n" name (string_of_float time);
	      flush chTime
	    end
	  else () (*ot not adds if the global var timeGath says so*)
   method com name =
	  if param.comment then
	    begin
	      Printf.fprintf chComm "%s \n" name ;
	      flush chComm
	    end
	  else () (*ot not adds if the global var timeGath says so*)
    method stat name =
	  if param.stats then 
	    begin
	      let time =Sys.time() in (*checkpoint the system time*)
	      Printf.fprintf chStat "%s \t %s\n" (string_of_float time) name ;
	      flush chStat
	    end
	  else () (*ot not adds if the global var timeGath says so*)

   method collected =	
	  close_out chTime;
	  close_out chStat;
	  close_out chComm
(* 	  close_out chData *)
   initializer
    begin
     match param.comment with
    | true -> chComm <- open_out fComm
    | false -> close_out chComm;
    match param.timing with
    | true -> chTime <- open_out fTime
    | false -> close_out chTime;
    match param.stats with
    | true -> chStat <- open_out fStat
    | false -> close_out chStat;
(*    match param.intern_data with
    | true -> chData <- open_out fDataR
    | false -> close_out chData;*)
   end
end;;



(*instantiation of the global object of information collection*)
let infO = new ingCollect collP;;

(* supporting funtions to gater statistics *)

let bddSizeS varN bdd= (string_of_int (Bdd.size bdd));;

let ssSizeS varN bdd=
(string_of_float (Bdd.nbminterms varN bdd));;

let stat2file varN bdd=
  match collP.stats with
  |false -> ignore 0
  |true  -> (infO#stat (bddSizeS varN bdd) ) 
;;