#load "str.cma";;
let inp_size = 0;;

let get_inpsize line = 
  List.length line ;; 

(*get_ words:
  takes in a string (line of a file here)
  and splits it on a space (" ") and stores the values in a list of strings 
*)
let get_words input = 
  Str.split_delim (Str.regexp " ") input
;;

let in_channel = open_in "num.txt" in
  try
    while true do
      let line = input_line in_channel in
        (*print_endline line*)
        print_endline line
    done;
  with End_of_file -> close_in in_channel;;

(*same as  intofstring*)
let change str = 
  int_of_string str ;;

let i = List.hd ["3"];;




(*each line is a string 
  this function reads each line and puts it in a list 

*)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
    loop [] ;;
read_lines "num.txt";;

let lines = read_lines "num.txt";;

(*can now index every line *)
List.nth lines 1 ;;

(*this function gives us the first element of our list of lines in the file 

  i.e the first line of the file 

  I need the first line of the list just to determine the number of inputs on a line i.e the number of streams -> bsically columns  *)
let x = List.hd (read_lines "num.txt") ;;


get_words x;;
(******TO NOTE: sometimes get_words counts end of line as a string so the actual length  will always be minus 1  *)
List.length (get_words x) ;;

let rec lengthacc acc = function
  |[] -> 0
  |[x] -> acc + 1
  |h :: t -> lengthacc (acc + 1)  t;;


let length list  = lengthacc 0 list;;

(*might not need *)
let actual_length list = 
  if List.length list = 1 then List.length list else 
    length list - 1 ;;

(*gets actual length of get_words*)
length (get_words x) ;;
