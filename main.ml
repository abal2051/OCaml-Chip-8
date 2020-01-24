open Base
open Stdio
open Chip8.Components

let binary = ref ""
let screen_width = ref 600
let screen_height = ref 600
let set_binary s = binary := s
let set_width w = screen_width := w  
let set_height h = screen_height := h  

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


let () =
  (* let arg_list = [ "-f", Caml.Arg.String set_binary, "Provide a Chip-8 ROM to run";] in
  let usage_msg = "Chip-8 Emulator" in
  Caml.Arg.parse arg_list print_endline usage_msg; *)
  let temp = ref "random.ch8" in 
  read_binary temp
  |> List.map ~f:Char.to_int
  |> Memory.load_ROM;
  CPU.init ();
  CPU.cycle ();
  (*
  List.map ~f:CPU.decode [ (0xA0, 00); (0x60, 0x10) ;(0xD0, 0x05) ]
  |> List.iter ~f:CPU.execute;
  let _ = Graphics.wait_next_event [Graphics.Button_down] in () *)
  (* CPU.cycle (); *)
  (* |> Utils.process_bytes *)
  (* |> List.map ~f:Instruction.decode *)
  (* |> List.map ~f:Instruction.to_string
  |> List.iter ~f:(fun str -> Stdio.printf "%s\n" str) *)
;;

(* let x = 
  let open Components.Framebuffer in 
  Stdio.printf "drawing..\n";
  init ();
  set 50 30 true;
  draw ();
  Graphics.wait_next_event [Graphics.Button_down]; *)

(* List.iter (fun byte -> printf "%x
" byte) bytes; *)
(* printf "%d
" (List.length bytes) *)