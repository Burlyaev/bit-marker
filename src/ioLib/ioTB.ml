open IoType


(* Pascal's first example with looped internal FSM *)
let b0P_TB =
	    [ {iLim="";stLim="0110";outLim="0";nowSt=0;nxtSt=1};(*Dummy init*)
	      {iLim="T";stLim="";outLim="0";nowSt=1;nxtSt=2};(*init*)
	      {iLim="T";stLim="";outLim="0";nowSt=2;nxtSt=3};(*reset state*)
	      {iLim="T";stLim="";outLim="1";nowSt=3;nxtSt=2}];;



(* created-dummy example to test output-patter in EPA *)

let b00_NO_TB =
	[{iLim="";stLim="TTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)		  	  {iLim="TT";stLim="";outLim="0";nowSt=1;nxtSt=2};(*init*)
	  {iLim="T1";stLim="";outLim="0";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="T0";stLim="";outLim="0";nowSt=3;nxtSt=4};
	  {iLim="T0";stLim="";outLim="1";nowSt=4;nxtSt=2}];;


let b00_TB =
	    [{iLim="";stLim="TTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	      {iLim="TT";stLim="";outLim="0";nowSt=1;nxtSt=2};(*init*)
	      {iLim="TT";stLim="";outLim="0";nowSt=2;nxtSt=3};(*reset state*)
	      {iLim="TT";stLim="";outLim="0";nowSt=2;nxtSt=2};
	      {iLim="T1";stLim="";outLim="0";nowSt=3;nxtSt=4};
	      {iLim="T0";stLim="";outLim="0";nowSt=4;nxtSt=5};
	      {iLim="T0";stLim="";outLim="1";nowSt=5;nxtSt=2}];;



(* ITC'99 *)
(*b01  --Finite state machine (FSM) comparing serial flows*)
let b01_TB= 
	      [{iLim="";stLim="TTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
		{iLim="TTTT";stLim="TTTTT";outLim="00";nowSt=1;nxtSt=2};(*init*)
		{iLim="10TT";stLim="";outLim="00";nowSt=2;nxtSt=3};(*reset state*)
		{iLim="01TT";stLim="";outLim="11";nowSt=3;nxtSt=3}];;

(* dummy deterministic execution*)
let b01_D_TB= 
	      [ {iLim="";stLim="11111";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
		{iLim="1111";stLim="";outLim="00";nowSt=1;nxtSt=2};(*init*)
		{iLim="1011";stLim="";outLim="00";nowSt=2;nxtSt=3};(*reset state*)
		{iLim="0111";stLim="";outLim="11";nowSt=3;nxtSt=3}];;


(*b02 -- Finite state machine (FSM) comparing serial flows  *)
let b02_TB= 
	    [{iLim="";stLim="TTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	      {iLim="TTT";stLim="";outLim="0";nowSt=1;nxtSt=2};(*init*)
	      {iLim="10T";stLim="";outLim="0";nowSt=2;nxtSt=3};(*reset state*)
	      {iLim="01T";stLim="";outLim="1";nowSt=3;nxtSt=3}];;

(* b03 -- Resource arbiter *)
let b03_TB= 
	      [{iLim="";stLim="TTTTTTTTTTTTTTTTTTTTTTTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
		{iLim="TTTTTT";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
		{iLim="10TTTT";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
		{iLim="01TTTT";stLim="";outLim="1111";nowSt=3;nxtSt=3}];;



(*b04 -- Compute min and max  - too heavy*)
let b04_TB =
	      [{iLim="";stLim="TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
		{iLim="TTTTTTTTTTT";stLim="";outLim="00000000";nowSt=1;nxtSt=2};(*init*)
		{iLim="10TTTTTTTTT";stLim="";outLim="00000000";nowSt=2;nxtSt=3};(*reset state*)
		{iLim="01TTTTTTTTT";stLim="";outLim="11111111";nowSt=3;nxtSt=3}];;



(*b06 -Interrupt handler *)
let b06_TB =
	  [{iLim="";stLim="TTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	    {iLim="TTTT";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
	    {iLim="10TT";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
	    {iLim="01TT";stLim="";outLim="1111";nowSt=3;nxtSt=3}];;


(*b07  --Count points on a straight line*)
let b07_TB =
	[{iLim="";stLim="TTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="TTT";stLim="";outLim="000000000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="10T";stLim="";outLim="000000000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="01T";stLim="";outLim="111111111";nowSt=3;nxtSt=3}];;

let b08_S_TB =[
	  {iLim="";stLim="TTTTTTTTTTTTTTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="TTTTTTTTT";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="10TTTTTTTTT";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="01TTTTTTTTT";stLim="";outLim="1111";nowSt=3;nxtSt=3}];;

(*b09-Serial to serial converter   *)	

let b09_TB =
	  [{iLim="";stLim="TTTTTTTTTTTTTTTTTTTTTTTTTTTT";outLim="0";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="TTT";stLim="";outLim="0";nowSt=1;nxtSt=2};(*init*)
	  {iLim="10T";stLim="";outLim="0";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="01T";stLim="";outLim="1";nowSt=3;nxtSt=3}];;

(* b11 *)
let b11_TB =
	[{iLim="";stLim="TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT";outLim="000000";nowSt=0;nxtSt=1};(*Dummy init*)
	      {iLim="TTTTTTTTT";stLim="";outLim="000000";nowSt=1;nxtSt=2};(*init*)
	      {iLim="10TTTTTTTT";stLim="";outLim="000000";nowSt=2;nxtSt=3};(*reset state*)
	      {iLim="01TTTTTTT";stLim="";outLim="111111";nowSt=3;nxtSt=3}];;

let b11_1_TB=
	      [{iLim="";stLim="TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="TTTTTTTTT";stLim="";outLim="";nowSt=1;nxtSt=2};(*init*)
	  {iLim="10TTTTTTTT";stLim="";outLim="";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="01TTTTTTT";stLim="";outLim="";nowSt=3;nxtSt=4};
	  {iLim="01TTTTTT0";stLim="";outLim="";nowSt=4;nxtSt=5};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=5;nxtSt=6};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=6;nxtSt=7};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=7;nxtSt=8};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=8;nxtSt=9};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=9;nxtSt=10};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=10;nxtSt=11};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=11;nxtSt=12};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=12;nxtSt=13};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=13;nxtSt=14};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=14;nxtSt=15};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=15;nxtSt=16};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=16;nxtSt=17};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=17;nxtSt=18};
	  {iLim="01TTTTTT1";stLim="";outLim="";nowSt=18;nxtSt=3}
	  ];;

(*b05 -- Elaborate the contents of a memory   *)
let b05_TB =[
	  {iLim="";stLim="TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="TTT";stLim="";outLim="000000000000000000000000000000000000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="100";stLim="";outLim="000000000000000000000000000000000000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="011";stLim="";outLim="000000000000000000000000000000000000";nowSt=3;nxtSt=4};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=4;nxtSt=5};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=5;nxtSt=6};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=6;nxtSt=7};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=7;nxtSt=8};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=8;nxtSt=9};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=9;nxtSt=10};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=10;nxtSt=11};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=11;nxtSt=12};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=12;nxtSt=13};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=13;nxtSt=14};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=14;nxtSt=15};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=15;nxtSt=16};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=16;nxtSt=17};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=17;nxtSt=18};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=18;nxtSt=19};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=19;nxtSt=20};(*init*)
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=20;nxtSt=21};(*reset state*)
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=21;nxtSt=22};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=22;nxtSt=23};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=23;nxtSt=24};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=24;nxtSt=25};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=25;nxtSt=26};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=26;nxtSt=27};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=27;nxtSt=28};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=28;nxtSt=29};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=29;nxtSt=30};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=30;nxtSt=31};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=31;nxtSt=32};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=32;nxtSt=33};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=33;nxtSt=34};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=34;nxtSt=35};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=35;nxtSt=36};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=36;nxtSt=37};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=37;nxtSt=38};(*init*)
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=38;nxtSt=39};(*reset state*)
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=39;nxtSt=40};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=40;nxtSt=41};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=41;nxtSt=42};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=42;nxtSt=43};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=43;nxtSt=44};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=44;nxtSt=45};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=45;nxtSt=46};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=46;nxtSt=47};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=47;nxtSt=48};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=48;nxtSt=49};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=49;nxtSt=50};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=50;nxtSt=51};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=51;nxtSt=52};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=52;nxtSt=53};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=53;nxtSt=54};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=54;nxtSt=55};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=55;nxtSt=56};(*init*)
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=56;nxtSt=57};(*reset state*)
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=57;nxtSt=58};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=58;nxtSt=59};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=59;nxtSt=60};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=61;nxtSt=61};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=62;nxtSt=62};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=63;nxtSt=63};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=64;nxtSt=64};
	  {iLim="010";stLim="";outLim="000000000000000000000000000000000000";nowSt=65;nxtSt=65};
	
	  {iLim="010";stLim="";outLim="111111111111111111111111111111111111";nowSt=66;nxtSt=3}
	  ];;



let b08_NO_TB =[
	  {iLim="";stLim="TTTTTTTTTTTTTTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="TTTTTTTTT";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="10TTTTTTTTT";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=3;nxtSt=4};
	  {iLim="011TTTTTTTT";stLim="";outLim="0000";nowSt=4;nxtSt=5};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=5;nxtSt=6};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=6;nxtSt=7};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=7;nxtSt=8};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=8;nxtSt=9};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=9;nxtSt=10};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=10;nxtSt=11};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=11;nxtSt=12};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=12;nxtSt=13};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=13;nxtSt=14};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=14;nxtSt=15};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=15;nxtSt=16};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=16;nxtSt=17};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=17;nxtSt=18};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=18;nxtSt=19};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=19;nxtSt=20};(*init*)
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=20;nxtSt=21};(*reset state*)
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=21;nxtSt=22};
	  {iLim="010TTTTTTTT";stLim="";outLim="1111";nowSt=22;nxtSt=3}
	  ];;

let b08_TB =[
	  {iLim="";stLim="TTTTTTTTTTTTTTTTTTTTT";outLim="";nowSt=0;nxtSt=1};(*Dummy init*)
	  {iLim="TTTTTTTTTTT";stLim="";outLim="0000";nowSt=1;nxtSt=2};(*init*)
	  {iLim="10TTTTTTTTT";stLim="";outLim="0000";nowSt=2;nxtSt=3};(*reset state*)
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=3;nxtSt=4};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=3;nxtSt=3};(*BACK LOOP*)
	  {iLim="011TTTTTTTT";stLim="";outLim="0000";nowSt=4;nxtSt=5};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=5;nxtSt=6};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=6;nxtSt=7};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=7;nxtSt=8};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=8;nxtSt=9};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=9;nxtSt=10};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=10;nxtSt=11};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=11;nxtSt=12};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=12;nxtSt=13};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=13;nxtSt=14};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=14;nxtSt=15};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=15;nxtSt=16};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=16;nxtSt=17};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=17;nxtSt=18};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=18;nxtSt=19};
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=19;nxtSt=20};(*init*)
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=20;nxtSt=21};(*reset state*)
	  {iLim="010TTTTTTTT";stLim="";outLim="0000";nowSt=21;nxtSt=22};
	  {iLim="010TTTTTTTT";stLim="";outLim="1111";nowSt=22;nxtSt=3}
	  ];;