(*creates list [0 ... (num-1)]*)
let rec enumF ?acc:(acc=[]) (num:int) =
  match (num) with 
  | 0 -> acc
  | _ -> enumF ~acc:([num-1]@acc) (num-1) 
;;


(* enumL --sup.func. creates the list of integers with the first element (startV) and the last element (startV+finishV)*)
let enumL startV finishV=
    let rec build_list lst finishV=
      let len=List.length lst in
      if (len<finishV ) then 
	build_list (List.append lst [(List.hd (List.rev lst)) +1]) finishV
      else 
	lst 
    in
  if (finishV =0) then [] else
  build_list [startV]  finishV;; 


(*function takes an integer and returns pair: (number of bits is bin representation;bin representation of int number)*)
type int2bin_T= {binLen:int;binForm: string};;

let rec int2binNum ?acc:(acc=1) ?str:(str="") num=
  let div= num/2 in
  let rest= (num mod 2) in
  let addStr=
      match rest with
      |1->"1"
      |_->"0"
  in
  if div>0 then
      int2binNum ~acc:(acc+1) ~str:(addStr^str) div
  else
    {
      binLen= acc;
      binForm= (addStr^str)
      }
    ;;

let wideBinStr (bin:string) (len:int) =
    (String.make (len- (String.length bin)) '0')^bin;;
    