open IoType

(* Pascal's first example with looped internal FSM *)
let b0P_Pr =
	      [	  {iLim="";stLim="zooz";outLim="0";nowSt=0;nxtSt=1}; (*Dummy init*)
		  {iLim="-";stLim="";outLim="0";nowSt=1;nxtSt=2};
		  {iLim="-";stLim="";outLim="0";nowSt=2;nxtSt=3};
		  {iLim="-";stLim="";outLim="1";nowSt=3;nxtSt=2} ];;
(* created-dummy example to test output-patter in EPA *)

let b00_NO_Pr =
	      [{iLim="";stLim="---";outLim="0";nowSt=0;nxtSt=1}; (*Dummy init*)
	       {iLim="--";stLim="";outLim="0";nowSt=1;nxtSt=2};
		{iLim="-o";stLim="";outLim="0";nowSt=2;nxtSt=3};
		{iLim="-z";stLim="";outLim="0";nowSt=3;nxtSt=4};
		{iLim="-z";stLim="";outLim="1";nowSt=4;nxtSt=2} ];;


let b00_Pr =
	      [{iLim="";stLim="---";outLim="0";nowSt=0;nxtSt=1}; (*Dummy init*)
	       {iLim="--";stLim="";outLim="0";nowSt=1;nxtSt=2};
		{iLim="--";stLim="";outLim="0";nowSt=2;nxtSt=3};
		{iLim="--";stLim="";outLim="0";nowSt=2;nxtSt=2};
		{iLim="-o";stLim="";outLim="0";nowSt=3;nxtSt=4};
		{iLim="-z";stLim="";outLim="0";nowSt=4;nxtSt=5};
		{iLim="-z";stLim="";outLim="1";nowSt=5;nxtSt=2}
		  ];;


(* ITC'99 *)
(*b01  --Finite state machine (FSM) comparing serial flows*)
let b01_Pr= 
		[{iLim="";stLim="zzzzz";outLim="00";nowSt=0;nxtSt=1}; (*Dummy init*)
		{iLim="zzzz";stLim="";outLim="00";nowSt=1;nxtSt=2};
		{iLim="oz";stLim="";outLim="00";nowSt=2;nxtSt=3};
		{iLim="zo";stLim="";outLim="11";nowSt=3;nxtSt=3}];;

(*b02 -- Finite state machine (FSM) comparing serial flows  *)
let b02_Pr= 
	    [{iLim="";stLim="oooo";outLim="0";nowSt=0;nxtSt=1}; (*Dummy init*)
	      {iLim="---";stLim="";outLim="0";nowSt=1;nxtSt=2};
	     {iLim="oz-";stLim="";outLim="0";nowSt=2;nxtSt=3};(*reset state*)
	     {iLim="zo-";stLim="";outLim="1";nowSt=3;nxtSt=3}];;

(* b03 -- Resource arbiter *)
let b03_Pr= 
[{iLim="";stLim="oooooooooooooooooooooooooooooo";outLim="0000";nowSt=0;nxtSt=1}; (*Dummy init*)
{iLim="------";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
  {iLim="oz----";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
  {iLim="zo----";stLim="";outLim="1111";nowSt=3;nxtSt=3}];;


(*b04 -- Compute min and max  - too heavy*)
let b04_Pr =
	      [{iLim="";stLim="oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
		{iLim="-----------";stLim="";outLim="00000000";nowSt=1;nxtSt=2};(*init*)
		{iLim="oz---------";stLim="";outLim="00000000";nowSt=2;nxtSt=3};(*reset state*)
		{iLim="zo---------";stLim="";outLim="11111111";nowSt=3;nxtSt=3}];;



(*b06 -Interrupt handler *)
let b06_Pr =
	  [{iLim="";stLim="ooooooooo";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	    {iLim="----";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
	    {iLim="oz--";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
	    {iLim="zo--";stLim="";outLim="1111";nowSt=3;nxtSt=3}];;


(*b07  --Count points on a straight line*)
let b07_Pr =
	[{iLim="";stLim="ooooooooo";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="---";stLim="";outLim="000000000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="oz-";stLim="";outLim="000000000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="zo-";stLim="";outLim="111111111";nowSt=3;nxtSt=3}];;

let b09_Pr =
	[{iLim="";stLim="ooooooooo";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="---";stLim="";outLim="000000000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="oz-";stLim="";outLim="000000000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="zo-";stLim="";outLim="111111111";nowSt=3;nxtSt=3}];;

let b11_Pr =
	[{iLim="";stLim="ooooooooooooooooooooooooooooooo";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="---------";stLim="";outLim="000000000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="oz--------";stLim="";outLim="000000000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="zo-------";stLim="";outLim="111111111";nowSt=3;nxtSt=3}];;



let b08_NO_Pr =[
	  {iLim="";stLim="ooooooooooooooooooooo";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="---------";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="oz---------";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=3;nxtSt=4};
	  {iLim="zoo--------";stLim="";outLim="0000";nowSt=4;nxtSt=5};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=5;nxtSt=6};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=6;nxtSt=7};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=7;nxtSt=8};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=8;nxtSt=9};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=9;nxtSt=10};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=10;nxtSt=11};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=11;nxtSt=12};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=12;nxtSt=13};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=13;nxtSt=14};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=14;nxtSt=15};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=15;nxtSt=16};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=16;nxtSt=17};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=17;nxtSt=18};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=18;nxtSt=19};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=19;nxtSt=20};(*init*)
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=20;nxtSt=21};(*reset state*)
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=21;nxtSt=22};
	  {iLim="zoz--------";stLim="";outLim="1111";nowSt=22;nxtSt=3}
	  ];;

let b08_Pr =[
	  {iLim="";stLim="ooooooooooooooooooooo";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="---------";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="oz---------";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=3;nxtSt=4};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=3;nxtSt=3};(*BACK LOOP*)
	  {iLim="zoo--------";stLim="";outLim="0000";nowSt=4;nxtSt=5};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=5;nxtSt=6};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=6;nxtSt=7};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=7;nxtSt=8};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=8;nxtSt=9};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=9;nxtSt=10};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=10;nxtSt=11};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=11;nxtSt=12};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=12;nxtSt=13};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=13;nxtSt=14};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=14;nxtSt=15};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=15;nxtSt=16};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=16;nxtSt=17};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=17;nxtSt=18};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=18;nxtSt=19};
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=19;nxtSt=20};(*init*)
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=20;nxtSt=21};(*reset state*)
	  {iLim="zoz--------";stLim="";outLim="0000";nowSt=21;nxtSt=22};
	  {iLim="zoz--------";stLim="";outLim="1111";nowSt=22;nxtSt=3}
	  ];;