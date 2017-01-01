(*
   #use "h.ml";; 
*)

(* Package loading *)
let hDir=Sys.getcwd();; 
let topDir="/home/burlyaev/Dropbox/_PHD_THESIS/Simulations/erra" ;; (*my intial directory *)
Sys.chdir topDir;;



(*loading of standart Strema module*)
    #load "str.cma";; 
    open Stream;;  
 
Sys.chdir topDir;;
(* loading the parcer fo ITC'99 benchmark format*)
    Sys.chdir "Build";; (*all files from Build- not locals*)

(* loading cudd *)
    #load "cudd.cma";;
    open Cudd;;
    open Format;;
    open Bdd;;

    #load "file.cmo";;  
    open File;;
    #load "strg.cmo";;  
    open Strg;;
print_string "1";;
         (*parser*)
    #load "itcBEN.cmo";;  
    open ItcBEN;;
    #load "graph.cmo";;
    open Graph;;


print_string "2";;

    #load "ioType.cmo";;  
    open IoType;;
(* IO-autom lib  *)
    #load "ioTB.cmo";;  
    open IoTB;;
    #load "ioPrecise.cmo";;  
    open IoPrecise;;
    #load "ioBits16.cmo";;  
    open IoBits16;;

(*    #load "autom_lib.cmo";;  
    open Autom_lib;;*)
(*    #load "b02_lib.cmo";;  
    open B02_lib;;*)

print_string "3";;
(* global variableles and parameters *)
    #load "timingGl.cmo";;  
    open TimingGl;;
(* circuit structrure constructions *)
    #load "cirG.cmo";;    
    open CirG;;
    #load "lst.cmo";;  
    open Lst;;
print_string "4";;
    #load "printGr.cmo";;
    open PrintGr;;
    #load "mvfs.cmo";;  
    open Mvfs;;
print_string "5";;
    #load "strucAlloc.cmo";;  
    open StrucAlloc;;


    #load "commOp.cmo";;  
    open CommOp;;

print_string "6";;
    #load "logOpM1.cmo";;  
    open LogOpM1;;
    #load "logOpM2.cmo";;  
    open LogOpM2;;
print_string "7";;
    #load "logOpM16.cmo";;  
    open LogOpM16;;
    #load "methChoice.cmo";;  
    open MethChoice;;

print_string "8";;
    #load "printCube.cmo";;  
    open PrintCube;;
    #load "formPairs.cmo";;  
    open FormPairs;;

    #load "bddShrink.cmo";;  
    open BddShrink;;
print_string "9";;
    #load "varResolv.cmo";;  
    open VarResolv;;
    #load "ssManip.cmo";;  
    open SsManip;;
    #load "num2Bdd.cmo";;  
    open Num2Bdd;;
    #load "inpAutom.cmo";;  
    open InpAutom;;
    #load "ioAutom.cmo";;  
    open IoAutom;;

    #load "cir2bdd.cmo";;  
    open Cir2bdd;;
print_string "10";;
    #load "bddTr.cmo";;  
    open BddTr;;
    #load "contamin.cmo";;  
    open Contamin;;
    #load "stopCond.cmo";;  
    open StopCond;;
    #load "step.cmo";;  
    open Step;;
    #load "genFun.cmo";;  
    open GenFun;;
print_string "11";;
    #load "fanInOut.cmo";;  
    open FanInOut;;
print_string "11.1";;
    #load "cirType.cmo";;  
    open CirType;;
print_string "11.2";;
    #load "model.cmo";;  
    open Model;; (*SET fault model calculation*)
print_string "11.3";;
    #load "votSuppr.cmo";;  
    open VotSuppr;;
print_string "11.4";;
    #load "buildCir.cmo";;  
    open BuildCir;;
print_string "11.5";;
    #load "top.cmo";;  
    open Top;;
print_string "12";;
(* Sys.chdir hDir;; *)
(*
   #use "h.ml";; 
*)