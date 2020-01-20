open Base
open Stdio

let binary = ref ""
let set_binary s = binary := s

let read_binary (filename : string ref) =
  let ch = In_channel.create !filename in
  let rec next buff =
    match In_channel.input_char ch with
    | Some b -> next (b :: buff)
    | None -> List.rev buff
  in
  let res = next [] in
  res
;;

let format_binary bytes =
  List.foldi
    ~f:(fun i str byte ->
      let byte = Char.to_int byte in
      if i % 8 = 0
      then Printf.sprintf "%s\n%02x" str byte
      else Printf.sprintf "%s %02x" str byte)
    ~init:""
    bytes
  ^ "\n"
;;

let main =
  let arg_list = [ "-f", Caml.Arg.String set_binary, "Provide a Chip-8 ROM to run" ] in
  let usage_msg = "Chip-8 Emulator" in
  Caml.Arg.parse arg_list print_endline usage_msg;
  let bytes = read_binary binary in
  printf "%s" (format_binary bytes)
;;

(* List.iter (fun byte -> printf "%x
" byte) bytes; *)
(* printf "%d
" (List.length bytes) *)
