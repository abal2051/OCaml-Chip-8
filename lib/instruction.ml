open Base
open Utils
open Components 

type instruction =
  | CLS (* 00 E0  *)
  | RND     of Register.t * int (* Cx kk  *)
  | LD_I    of Register.t * int (* an nn  *)
  | LD_B    of Register.t * Register.t (* Fx 33  *)
  | LD_R_I  of Register.t * Register.t (* Fx 65  *)
  | LD_I_R  of Register.t * Register.t (* Fx 55  *)
  | LD_F    of Register.t * Register.t (* Fx 29  *)
  | LD_R_B  of Register.t * int (* 6x kk  *)
  | LD_K    of Register.t (* Fx 0a *)
  | DRW     of Register.t * Register.t * int (* Dx yn *)
  | JP      of int (* 1n nn *)

let decode = function
  | 0x0, 0x0, 0xE, 0x0        -> CLS
  | 0xC, x, k_high, k_low     -> RND (Register.v x, merge_bits k_high k_low)
  | 0xA, k_high, k_mid, k_low -> LD_I (Register.i, merge_bits3 k_high k_mid k_low)
  | 0xF, x, 0x3, 0x3          -> LD_B (Register.i, Register.v x)
  | 0xF, x, 0x6, 0x5          -> LD_R_I (Register.v x, Register.i)
  | 0xF, x, 0x5, 0x5          -> LD_I_R (Register.i, Register.v x)
  | 0xF, x, 0x2, 0x9          -> LD_F (Register.i, Register.v x)
  | 0x6, x, k_high, k_low     -> LD_R_B (Register.v x, merge_bits k_high k_low)
  | 0xF, x, 0x0, 0xa          -> LD_K (Register.v x)
  | 0xD, x, y, n              -> DRW (Register.v x, Register.v y, n)
  | 0x1, k_high, k_mid, k_low -> JP (merge_bits3 k_high k_mid k_low)
  | _ -> failwith "Unknown opcode encountered"
;;

let to_string = 
  function
  | CLS             -> "CLS" (* 00 E0  *)
  | RND    (r,  x)  -> Printf.sprintf "RND%6x,%7x" (Register.to_int r) x (* Cx kk  *)
  | LD_I   (_,  x)  -> Printf.sprintf "LD%7s,%7x" "I" x(* an nn  *)
  | LD_B   (_, r)   -> Printf.sprintf "LD%7s,%7x" "B" (Register.to_int r)(* Fx 33  *)
  | LD_R_I (r, _)   -> Printf.sprintf "LD%7x,%7s" (Register.to_int r) "I" (* Fx 65  *)
  | LD_I_R (_, r)   -> Printf.sprintf "LD%7s,%7x" "I" (Register.to_int r)(* Fx 55  *)
  | LD_F   (_, r)   -> Printf.sprintf "LD%7s,%7x" "F" (Register.to_int r)(* Fx 29  *)
  | LD_R_B (r, k)   -> Printf.sprintf "LD%7x,%7x"(Register.to_int r) k (* 6x kk  *)
  | LD_K   r        -> Printf.sprintf "LD%7x,%7s" (Register.to_int r) "K" (* Fx 0a *)
  | DRW   (x, y, n) -> Printf.sprintf "DRW%6x,%7x,%7x" (Register.to_int x) (Register.to_int y) n (* Dx yn *)
  | JP     n        -> Printf.sprintf "JP%7x" n(* 1n nn *)