open Cudd
open StrucAlloc
open MethChoice
open Cir2bdd
open CirG
open IoType
open FanInOut
open TimingGl
open GenFun

(*just type to return cirBuild*)
type cirBu_T =
  {
    cirBu:cirBdd_T;
    grBu:grL_T;
    priorBu: fanPoint_T list;
    iniStBu: v_T;
    fixStB: v_T
    }
;;