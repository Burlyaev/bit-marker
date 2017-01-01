

(*read_file takes the name of the file to read it and returns the string list of the files lines*)

let read_file filename = 
  let chan = 
      try
	open_in filename 
      with _ ->  begin 
		    let ch=  open_out filename in
		    close_out ch; 	
		    open_in filename 
		end
  in
  let rec ch2lines accumL	=
    try
      let newAccum= List.append accumL [input_line chan] in
      ch2lines newAccum
    with End_of_file ->	
	close_in chan;
	accumL
  in
  ch2lines [];;

	

(*list_to_file-- function that tkaes string list as an input and write this list to the newly created file with the name filename*)	

let list_to_file filename line_list =
    let chan_out = open_out filename in
    let line_to_file channel line=  
	    output_string chan_out (line^"\n")
    in
    let write_list= line_to_file chan_out in
  List.iter write_list line_list;
  close_out chan_out;;