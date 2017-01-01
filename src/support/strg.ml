
(* str2chL --suo.func. convers string to the list of chars *)
(*let str2chL (s:string) =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let lst2str lst=
  let strL= List.map (fun num-> (string_of_int num)^"; ") lst in
  List.fold_left (fun a b-> a^b) "" strL;;


  chL2str -sup.func. merges the char list to a single string*)
(*    let  chL2str chL=
      let rec ch2str chL accum=
	if ((List.length chL)>0) then
	    ch2str (List.tl chL) (accum ^ Char.escaped (List.hd chL))
	else 
	    accum
      in
    ch2str chL "";;
*) 