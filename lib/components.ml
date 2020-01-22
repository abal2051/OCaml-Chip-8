open Base

module Register : sig
  type t

  val file : t array
  val i : t
  val vf : t
  val delay : t
  val time : t
  val pc : t
  val sp : t
  val v : int -> t
  val to_int : t -> int
  val write : t -> int -> unit
  val read : t -> int
end = struct
  type t = int

  let file = Array.create ~len:22 0
  let i = 16
  let vf = 17
  let delay = 18
  let time = 19
  let pc = 20
  let sp = 21
  let v indx = indx
  let to_int reg = reg
  let write reg value = file.(reg) <- value
  let read reg = file.(reg)
end

module Memory : sig
  type t

  val mem : t
  val write : int -> int -> unit
  val read : int -> int
end = struct
  type t = int array

  let mem = Array.create ~len:4096 0
  let write addr value = mem.(addr) <- value
  let read addr = mem.(addr)
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
    let index = (64 * y) + x in buf.(index) <- pix
  ;;
  
  let clear () = Graphics.clear_graph ()
  
  let draw () =
    Array.iteri buf ~f:(fun i pix ->
        let x = i % 64 in
        let y = i / 64 in
        if pix then Graphics.fill_circle x y 5)
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
