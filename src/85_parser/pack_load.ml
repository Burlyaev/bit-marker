(* Package loading *)
let curDir=Sys.getcwd();; (* my intial directory of main.ml top level prgram- save it here because after that I am jumping through folders *)
#load "str.cma";;
#use "parser_itcBEN.ml";;
#use "ckt_to_l2hdl.ml" ;;
#use "compare.ml";;


#use "file_sort.ml";;
#use "file_names.ml";;

#use "bin_int_conv.ml";;




#load "nums.cma";;
open Nat;;  
open Big_int;;
(*transfrom.ml load manually *)

(* proc. to use graph_build  *)
Sys.chdir "lib/ocamlgraph/";;
#load "graph.cma";;
open Graph;;(* open Graph.Pack.Digraph;; *)
open Path;;
Sys.chdir curDir;;

(* procedure to use cudd interface cudd.cma *)
Sys.chdir "../packs_libs/MLCuddIDL/trunk/";;
#load "cudd.cma";;
open Cudd;;
open Format;;
Sys.chdir curDir;;


#use "topfind";;
#require "batteries";;
(*Sys.chdir "/usr/lib/ocaml/batteries/";;
open Batteries;;
#load "batteries.cma";;
(* open Graph;; *)
(* open Path;; *)
Sys.chdir curDir;;*)

#use "graph_build.ml";;
#use "combTOseq2.ml";;



#use "ioLST.ml";;
#use "combTOseq.ml";;

#use "optim_dist.ml";;
(* my advanced amrking algorithm- check and compare *)
#use "dist_marking.ml";;
(* #use "dist_marking_rec.ml" *)
#use "optimal_sol.ml";;

#use "optim_path.ml";;
#use "optim_pascal.ml";;

#use "optim_pas_ms.ml";;

#use "transform.ml";; 
#use "tmr.ml";; 



(* procedure to use ExtLib library for cyc_dig.ml *)
(* Sys.chdir "../packs_libs/extlib-1.5.3/";; *)
(* #load "extLib.cma";; *)


#use "topfind";;
#require "extlib";;
open ExtLib;;
open ExtString;;
#use "optim_pas_ms2.ml";;
(* Sys.chdir curDir;; *)

#use "cyc_dig.ml";; 
#use "cyc_digG.ml";; 

#use "optim_pas_ms_tune.ml";;
(* #use "mixed_voINS.ml";; *)

#use "sup_check_fun.ml";;
#use "sup_check_fun2.ml";;
#use "sup_check_fun3.ml";;
 
#use "stat_gathering.ml";;
#use "sup_check_fun3.ml";;


(* proc. to use gnu_lot  *)
#use "topfind";;
#require "gnuplot";;

(* EOF *)