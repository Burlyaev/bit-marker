open TimingGl
open BuildCir
open AccumAn
open PropagAn

    let a=1;;
    print_string "[Echo]Program runs...\n";;

    infO#tim ("ERRA:Start::");;

    let cirBddu= cirBuild;;

    let minVL= 
		match collP.analys with 
		|EAA ->votReducEAA cirBddu
		|EPA ->votReducEPA cirBddu
    ;;




infO#tim ("ERRA:Global Finish::");;
(*  *)

let voterStr= List.fold_left (fun s n-> s^n^"\n")"[DONE]Voters Left::\n" minVL ;;
infO#com voterStr;;
infO#com ("[DONE]Voters Number::"^(string_of_int (List.length minVL)));;



infO#collected;;