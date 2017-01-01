open Cudd
open TimingGl
open CirG
open StrucAlloc
open MethChoice
open Format
open FormPairs

(*calculates pairs: (vertex name, [names of succ verts] )*)
let pairSET (grStr:grL_T) =
  let grSeq = grStr.seqI in (*we take seq graph structure with inputs*)
  (*inputs are needeed since they also can be corrupted in comb circ*)
  let gr = grSeq.gr in (*graph structure*)
  (*find succ vertices for each vertex of seq graph*)
  let vertSucPL = List.map  (*pairs: (vertex name, [names of succ verts] )*)
    (fun v -> (G.V.label v, List.map (fun vs->G.V.label vs) (G.succ gr v) )) grSeq.vGL 
  in
  vertSucPL;;

(*power set calculation- all possible subsets of xs set- for SET modelling*)
let subsets xs = List.fold_right 
		    (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs 
		 [[]];;


(*SET introduction in the cone after varBdd memory cell or input*)
let setInj (cirBdd:cirBdd_T) (grStr:grL_T) (varBdd:vBDD_T) stSp=
  let pairSET= pairSET grStr in (*calcuale injection-cone combinations*)
infO#tim ("[setInj] injection cones found");
  (*find pair (injPoint, successors)*)
  let propPair = List.find (fun pair-> (fst pair)=varBdd.vname) pairSET in
  let succNamL= snd propPair in (*names of successors to corrupt*)
infO#tim ("[setInj] an injection cone found: its size= "^string_of_int(List.length succNamL));  

  let bddLF nameL= List.map (*fun find bddStr of successors to corrupt*)
	    (fun name-> 	
	      List.find (fun bdd-> bdd.vname=name) (cirBdd.memVar) (*cirBdd.insVar@- deleted*)
	    ) 
	    nameL in 
  (*now we have to corrupt this set in all possible ways*)
  (*for optim. all possible set is calculated on names*)
  let combinNamLL= subsets succNamL in	
infO#tim ("[setInj] power set is calculated:: ");
(*  let errCube namL =
	List.fold_left (fun nam-> 
				let corBdd= 
				      List.find (fun bdd-> bdd.vname=nam) cirBdd.memVar in
				seuInj cirBdd acc

	) stSp namL*) 
    (*accrod to combins we intro SEUs     *)
  let combinBddL= List.map (*for each combin*)
		  (fun combL-> 
		      let bddL= bddLF combL in (*bdd list where inj SEU*)
	List.fold_left (fun acc bdd-> seuInj bdd acc ) 
	    stSp bddL    
		  )
		 combinNamLL in
infO#tim ("[setInj] before ORing- the end:: ");
  (*curruptions are mutually exclusive- so OR of them*)
  List.fold_left (Bdd.dor) (Bdd.dfalse cirBdd.manBdd) combinBddL;;



(* variation of function that intro SET to all possible cones- mutually exclusively  *)
let setInjG (cirBdd:cirBdd_T) (grStr:grL_T) stSp= 
  let conesPointL= (cirBdd.insVar @ cirBdd.memVar) in (*cones starts at inputs and mems*)
  let errConeL = List.map (fun cone -> setInj cirBdd grStr cone stSp) conesPointL in (*for each-injection*)

List.iter (fun errCone -> bdd2file cirBdd errCone) errConeL;
dFileV "sep2join";
infO#tim ("[setInjG] before OR-ing::"); 

  List.fold_left 
      (fun acc errCone-> 
       Bdd.dor acc errCone
      ) 
  ( Bdd.dfalse cirBdd.manBdd) errConeL;;

(* variation of function that intro SEU to all memory cells- mutually exclusively  *)
let seuInjG (cirBdd:cirBdd_T) stSp=
  let injPoints= cirBdd.memVar in
  let errMemL= List.map (fun bdd -> seuInj bdd stSp) injPoints in
List.iter (fun errMem -> bdd2file cirBdd errMem) errMemL;
dFileV "sep2join";
  List.fold_left 
    (fun acc errMem -> Bdd.dor acc errMem) 
  (Bdd.dfalse cirBdd.manBdd )  errMemL ;;




(*
statPairSet "Bench/b01_opt_r.bench";;
*)
(*function that is not used later but gathers statistics for understanding*)
(* let statPairSet (grStr:grL_T) =
    let pairsL= pairSET grStr in
    let grSeq= setGraSeq filename in 
    let numInp= List.length grSeq.inpVertL in
    let numMem= (G.nb_vertex grSeq.graph) - numInp in
    let parNumL = List.map (fun par -> (fst par, List.length (snd par))) pairsL in
    let perNumL = List.map (fun par -> (fst par, (float (List.length (snd par)))/.( float numMem))) pairsL in
    parNumL;;*)
      