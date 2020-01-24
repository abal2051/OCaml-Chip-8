open Base
open Utils

module Register : sig
  type t

  val file : t array
  val i : t
  val vf : t
  val delay : t
  val time : t
  val pc : t

  (* val sp : t *)
  val v : int -> t
  val to_int : t -> int
  val write : t -> int -> unit
  val read : t -> int
  val iteri : f:(int -> t -> unit) -> t -> unit
  val dump : unit -> unit
end = struct
  type t = int

  let file = Array.create ~len:22 0
  let i = 16
  let vf = 17
  let delay = 18
  let time = 19
  let pc = 20

  (* let sp = 21 *)
  let iteri ~f vx = Array.filteri file ~f:(fun i _ -> i <= vx) |> Array.iteri ~f
  let v indx = indx
  let to_int reg = reg
  let write reg value = file.(reg) <- value
  let read reg = file.(reg)

  let dump () =
    Array.iteri file ~f:(fun i _ -> Stdio.printf "Register %d has %d\n" i (read i))
  ;;
end

module Memory : sig
  type t

  val init : unit -> unit
  val mem : t
  val write : addr:int -> int -> unit
  val read : int -> int
  val dump_ROM : unit -> unit
  val load_ROM : int list -> unit
  val rom_size : int ref
  val dump : unit -> unit
end = struct
  type t = int array

  let rom_size = ref 0
  let mem = Array.create ~len:4096 0
  let write ~addr value = mem.(addr) <- value
  let read addr = mem.(addr)

  let init () =
    write ~addr:0 0xF0;
    write ~addr:1 0x90;
    write ~addr:2 0x90;
    write ~addr:3 0x90;
    write ~addr:4 0xF0;
    write ~addr:5 0x20;
    write ~addr:6 0x60;
    write ~addr:7 0x20;
    write ~addr:8 0x20;
    write ~addr:9 0x70;
    write ~addr:10 0xF0;
    write ~addr:11 0x10;
    write ~addr:12 0xF0;
    write ~addr:13 0x80;
    write ~addr:14 0xF0;
    write ~addr:15 0xF0;
    write ~addr:16 0x10;
    write ~addr:17 0xF0;
    write ~addr:18 0x10;
    write ~addr:19 0xF0;
    write ~addr:20 0x90;
    write ~addr:21 0x90;
    write ~addr:22 0xF0;
    write ~addr:23 0x10;
    write ~addr:24 0x10;
    write ~addr:25 0xF0;
    write ~addr:26 0x80;
    write ~addr:27 0xF0;
    write ~addr:28 0x10;
    write ~addr:29 0xF0;
    write ~addr:30 0xF0;
    write ~addr:31 0x80;
    write ~addr:32 0xF0;
    write ~addr:33 0x90;
    write ~addr:34 0xF0;
    write ~addr:35 0xF0;
    write ~addr:36 0x10;
    write ~addr:37 0x20;
    write ~addr:38 0x40;
    write ~addr:39 0x40;
    write ~addr:40 0xF0;
    write ~addr:41 0x90;
    write ~addr:42 0xF0;
    write ~addr:43 0x90;
    write ~addr:44 0xF0;
    write ~addr:45 0xF0;
    write ~addr:46 0x90;
    write ~addr:47 0xF0;
    write ~addr:48 0x10;
    write ~addr:49 0xF0;
    write ~addr:50 0xF0;
    write ~addr:51 0x90;
    write ~addr:52 0xF0;
    write ~addr:53 0x90;
    write ~addr:54 0x90;
    write ~addr:55 0xE0;
    write ~addr:56 0x90;
    write ~addr:57 0xE0;
    write ~addr:58 0x90;
    write ~addr:59 0xE0;
    write ~addr:60 0xF0;
    write ~addr:61 0x80;
    write ~addr:62 0x80;
    write ~addr:63 0x80;
    write ~addr:64 0xF0;
    write ~addr:65 0xE0;
    write ~addr:66 0x90;
    write ~addr:67 0x90;
    write ~addr:68 0x90;
    write ~addr:69 0xE0;
    write ~addr:70 0xF0;
    write ~addr:71 0x80;
    write ~addr:72 0xF0;
    write ~addr:73 0x80;
    write ~addr:74 0xF0;
    write ~addr:75 0xF0;
    write ~addr:76 0x80;
    write ~addr:77 0xF0;
    write ~addr:78 0x80;
    write ~addr:79 0x80
  ;;

  let dump_ROM () =
    let s =
      Utils.format_bytes
        (List.filteri
           ~f:(fun i _ -> i >= 0x200 && i < 0x200 + !rom_size)
           (Array.to_list mem))
    in
    Stdio.printf "%s" s
  ;;

  let dump () =
    let s =
      Utils.format_bytes (List.filteri ~f:(fun i _ -> i >= 0x200) (Array.to_list mem))
    in
    Stdio.printf "%s" s
  ;;

  let load_ROM instructions =
    List.iteri instructions ~f:(fun i instr ->
        write ~addr:(0x200 + i) instr;
        rom_size := !rom_size + 1)
  ;;
end

module Addr_Stack : sig
  type t

  val stack : t
  val push : int -> unit
  val pop : unit -> int
end = struct
  type t = int Stack.t

  let stack = Stack.create ()

  let push addr =
    if Stack.length stack < 16 then Stack.push stack addr else failwith "Stack overflow"
  ;;

  let pop () =
    match Stack.pop stack with
    | Some addr -> addr
    | None -> failwith "should this be an invalid state?"
  ;;
end

module Framebuffer : sig
  type t

  val buf : t
  val init : unit -> unit
  val set : int -> int -> bool -> unit
  val clear : unit -> unit
  val draw : unit -> unit
end = struct
  type t = bool array

  let buf = Array.create ~len:2048 false
  let init () = Graphics.open_graph " 640x320"

  let set x y pix =
    let index = (64 * y) + x in
    let xor_res = Utils.xor buf.(index) pix in
    if Bool.( = ) buf.(index) true && Bool.( = ) xor_res false
    then Register.write Register.vf 1
    else Register.write Register.vf 0;
    buf.(index) <- xor_res
  ;;

  let clear () =
    Graphics.clear_graph ();
    Array.iteri buf ~f:(fun i _ -> buf.(i) <- false)
  ;;

  let draw () =
    Array.iteri buf ~f:(fun i pix ->
        let x = i % 64 * 10 in
        let y = i / 64 * 10 in
        if pix then Graphics.fill_rect x (312 - y) 8 8)
  ;;
end

module CPU : sig
  type instruction

  val init : unit -> unit
  val fetch : unit -> int * int
  val incr_pc : unit -> unit
  val decode : int * int -> instruction
  val execute : instruction -> unit
  val to_string : instruction -> string
  val cycle : unit -> unit

  (* val execute: Instruction.t -> unit *)
end = struct
  type instruction =
    | CLS (* 00 E0  *)
    | RND of Register.t * int (* Cx kk  *)
    | LD_I of Register.t * int (* an nn  *)
    | LD_B of Register.t * Register.t (* Fx 33  *)
    | LD_R_I of Register.t * Register.t (* Fx 65  *)
    | LD_I_R of Register.t * Register.t (* Fx 55  *)
    | LD_F of Register.t * Register.t (* Fx 29  *)
    | LD_R_B of Register.t * int (* 6x kk  *)
    | LD_K of Register.t (* Fx 0a *)
    | DRW of Register.t * Register.t * int (* Dx yn *)
    | JP of int

  (* 1n nn *)

  let decode (higher_byte, lower_byte) =
    Utils.make_byte_seq higher_byte lower_byte
    |> function
    | 0x0, 0x0, 0xE, 0x0 -> CLS
    | 0xC, x, k_high, k_low -> RND (Register.v x, merge_bits k_high k_low)
    | 0xA, k_high, k_mid, k_low -> LD_I (Register.i, merge_bits3 k_high k_mid k_low)
    | 0xF, x, 0x3, 0x3 -> LD_B (Register.i, Register.v x)
    | 0xF, x, 0x6, 0x5 -> LD_R_I (Register.i, Register.v x)
    | 0xF, x, 0x5, 0x5 -> LD_I_R (Register.i, Register.v x)
    | 0xF, x, 0x2, 0x9 -> LD_F (Register.i, Register.v x)
    | 0x6, x, k_high, k_low -> LD_R_B (Register.v x, merge_bits k_high k_low)
    | 0xF, x, 0x0, 0xa -> LD_K (Register.v x)
    | 0xD, x, y, n -> DRW (Register.v x, Register.v y, n)
    | 0x1, k_high, k_mid, k_low -> JP (merge_bits3 k_high k_mid k_low)
    | _ -> failwith "Unknown opcode encountered"
  ;;

  let to_string = function
    | CLS -> "CLS" (* 00 E0  *)
    | RND (r, x) -> Printf.sprintf "RND%6x,%7x" (Register.to_int r) x (* Cx kk  *)
    | LD_I (_, x) -> Printf.sprintf "LD_I%5s,%7x" "I" x (* an nn  *)
    | LD_B (_, r) -> Printf.sprintf "LD_B%5s,%7x" "B" (Register.to_int r) (* Fx 33  *)
    | LD_R_I (r, _) -> Printf.sprintf "LD_R_I%3x,%7s" (Register.to_int r) "I" (* Fx 65  *)
    | LD_I_R (_, r) -> Printf.sprintf "LD_I_R%3s,%7x" "I" (Register.to_int r) (* Fx 55  *)
    | LD_F (_, r) -> Printf.sprintf "LD_F%5s,%7x" "F" (Register.to_int r) (* Fx 29  *)
    | LD_R_B (r, k) -> Printf.sprintf "LD_R_B%3x,%7x" (Register.to_int r) k (* 6x kk  *)
    | LD_K r -> Printf.sprintf "LD_K%5x,%7s" (Register.to_int r) "K" (* Fx 0a *)
    | DRW (x, y, n) ->
      Printf.sprintf "DRW%6x,%7x,%7x" (Register.to_int x) (Register.to_int y) n
    (* Dx yn *)
    | JP n -> Printf.sprintf "JP%7x" n
  ;;

  let init () =
    Register.write Register.pc 0x200;
    Framebuffer.init ();
    Memory.init ()
  ;;

  let fetch () =
    Memory.read (Register.read Register.pc), Memory.read (Register.read Register.pc + 1)
  ;;

  let incr_pc () = Register.write Register.pc (Register.read Register.pc + 2)

  let execute instr =
    match instr with
    | CLS -> Framebuffer.clear ()
    | RND (vx, kk) -> Register.write vx (Random.int 256 land kk)
    | LD_I (vx, nn) -> Register.write vx nn
    | LD_B (i, vx) ->
      let vx = Register.read vx in
      let huns = vx / 100 in
      let tens = (vx - (huns * 100)) / 10 in
      let ones = vx - (huns * 100) - (tens * 10) in
      let i = Register.read i in
      Memory.write ~addr:i huns;
      Memory.write ~addr:(i + 1) tens;
      Memory.write ~addr:(i + 2) ones
    | LD_R_I (i, vx) ->
      let i = Register.read i in
      Register.iteri vx ~f:(fun count _ ->
          Stdio.printf "%d\n" count;
          Register.write (Register.v count) (Memory.read (i + count)))
    | LD_I_R (i, vx) ->
      let i = Register.read i in
      Register.iteri vx ~f:(fun count r ->
          Memory.write ~addr:(i + count) (Register.read r))
    | LD_F (i, vx) ->
      let vx = Register.read vx in
      Register.write i (vx * 5)
    | LD_R_B (vx, b) -> Register.write vx b
    | LD_K vx ->
      let rec check_key () =
        let { Graphics.key = seen_key; _ } =
          Graphics.wait_next_event [ Graphics.Key_pressed ]
        in
        if not (List.exists Utils.accepted_keys ~f:(fun k -> Char.equal k seen_key))
        then check_key ()
        else Register.write vx (Char.to_int seen_key)
      in
      check_key ()
    | DRW (vx, vy, nn) ->
      let addr = Register.read Register.i in
      let rec aux x y nn addr =
        if nn = 0
        then ()
        else (
          let sprite_row = Memory.read addr in
          let bit_arr = Utils.byte_to_bools sprite_row in
          (* let bit_sexp = Sexp.to_string ([%sexp_of: bool list] bit_arr) in   *)
          List.iteri bit_arr ~f:(fun i bit -> Framebuffer.set (x + i) y bit);
          aux x (y + 1) (nn - 1) (addr + 1))
      in
      aux (Register.read vx) (Register.read vy) nn addr
    | JP addr -> Register.write Register.pc addr
  ;;

  (* | _ -> failwith "not imp" *)

  let rec cycle () =
    let fetched = fetch () in
    incr_pc ();
    let decoded = decode fetched in
    (* let b1, b2 = fetched in *)
    (* Stdio.printf "%x %x
" b1 b2;
    Stdio.printf "%s
" (to_string decoded);
    Register.dump (); *)
    (* Stdio.printf "------------------------------
";
    Stdio.Out_channel.flush Stdio.stdout; *)
    execute decoded;
    Framebuffer.draw ();
    Unix.sleepf 0.1;
    cycle ()
  ;;
end

(* 
Spirte memory requirements:
16 sprites, each requireing 5 bytes
so going from 0x00 to 0x50
Memory Map:
+---------------+= 0xFFF (4095) End of Chip-8 RAM
|               |
|               | 
|               |
|               |
|               |
| 0x200 to 0xFFF| 
|     Chip-8    |
| Program / Data|
|     Space     |
|               |
|               |
|               |
|               |
|               |
|               |
|               |
+---------------+= 0x200 (512) Start of most Chip-8 programs
| 0x000 to 0x1FF|
| Reserved for  |
|  interpreter  |
+---------------+= 0x000 (0) Start of Chip-8 RAM
drr
*)
