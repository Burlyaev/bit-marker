type ioType_T=
{
  iLim:  string;
  stLim: string; 
  outLim: string;
  nowSt: int;
  nxtSt: int 
}


(*returns the max seq num of state in io autom*)
let maxStA (aut: ioType_T list) =
  let allStL= List.concat( List.map(fun st-> [st.nowSt; st.nxtSt]) aut )in
  let sortL= List.sort (fun a b -> b-a) allStL in
  List.hd sortL;;


