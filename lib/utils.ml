open Base

let lower_bits byte = byte land 0x0F
let upper_bits byte = byte lsr 4

let accepted_keys =
  [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' ]
;;

let byte_to_bools byte =
  let l =
    [ byte land 1 > 0
    ; byte land 2 > 0
    ; byte land 4 > 0
    ; byte land 8 > 0
    ; byte land 16 > 0
    ; byte land 32 > 0
    ; byte land 64 > 0
    ; byte land 128 > 0
    ]
  in
  List.rev l
;;

let xor x y = (x && not y) || (y && not x)
let merge_bits b_high b_low = (b_high lsl 4) + b_low
let merge_bits3 b_high b_mid b_low = merge_bits (merge_bits b_high b_mid) b_low
let make_byte_seq b1 b2 = upper_bits b1, lower_bits b1, upper_bits b2, lower_bits b2

let process_bytes bytes =
  let rec aux processed = function
    | b1 :: b2 :: tl -> aux (make_byte_seq b1 b2 :: processed) tl
    | [] -> List.rev processed
    | [ _ ] -> failwith "Chip-8 binaries should have an even number of bytes"
  in
  aux [] bytes
;;

let format_bytes bytes =
  List.foldi
    ~f:(fun i str byte ->
      let byte = byte in
      if i % 8 = 0
      then Printf.sprintf "%s\n%02x" str byte
      else Printf.sprintf "%s %02x" str byte)
    ~init:"ROM dump..\n------------"
    bytes
  ^ "\n"
;;

let%test "byte_to_bool" =
  List.equal
    Bool.equal
    (byte_to_bools 11)
    [ false; false; false; false; true; false; true; true ]
;;

let%test "upper_bits" = upper_bits 0x32 = 3 (* 0x32 -> 0x03 *)
let%test "lower_bits" = lower_bits 0x32 = 2 (* 0x32 -> 0x02 *)
let%test "upper_bits" = upper_bits 0x001F = 1 (* 0x32 -> 0x01 *)
let%test "lower_bits" = lower_bits 0x001F = 15 (* 0x32 -> 0x0F *)
let%test "merge_bits (2 units)" = merge_bits 0x1 0x2 = 0x12
let%test "merge_bits_3 (3 units)" = merge_bits3 0x2 0x6 0x5 = 0x265

let%test "byte_seq1" =
  [%compare: int * int * int * int] (make_byte_seq 0xc0 0xff) (0xc, 0x0, 0xf, 0xf) = 0
;;

(* 0xc0ff -> 0xc 0x0 0xf 0xf*)

let%test "byte_seq2" =
  [%compare: int * int * int * int] (make_byte_seq 0x12 0x00) (0x1, 0x2, 0x0, 0x0) = 0
;;

(* 0x1200 -> 0x1 0x2 0x0 0x0*)

let%test "process_bytes" =
  [%compare: (int * int * int * int) list]
    (process_bytes [ 0x00; 0xe0; 0xc0; 0xff ])
    [ 0x0, 0x0, 0xe, 0x0; 0xc, 0x0, 0xf, 0xf ]
  = 0
;;
