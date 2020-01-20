open Base
open Memory
open Utils

type instruction =
  | CLS (* 00 E0  *)
  | RND     of register * int (* Cx kk  *)
  | LD_I    of long_register * int (* an nn  *)
  | LD_B    of long_register * register (* Fx 33  *)
  | LD_R_I  of register * long_register (* Fx 65  *)
  | LD_I_R  of long_register * register (* Fx 55  *)
  | LD_F    of long_register * register (* Fx 29  *)
  | LD_R_B  of register * int (* 6x kk  *)
  | LD_K    of register (* Fx 0a *)
  | DRW     of register * register * int (* Dx yn *)
  | JP      of int (* 1n nn *)

let decode = function
  | 0x0, 0x0, 0xE, 0x0        -> CLS
  | 0xC, x, k_high, k_low     -> RND (reg x, merge_bits k_high k_low)
  | 0xA, k_high, k_mid, k_low -> LD_I (long_reg, merge_bits3 k_high k_mid k_low)
  | 0xF, x, 0x3, 0x3          -> LD_B (long_reg, reg x)
  | 0xF, x, 0x6, 0x5          -> LD_R_I (reg x, long_reg)
  | 0xF, x, 0x5, 0x5          -> LD_I_R (long_reg, reg x)
  | 0xF, x, 0x2, 0x9          -> LD_F (long_reg, reg x)
  | 0x6, x, k_high, k_low     -> LD_R_B (reg x, merge_bits k_high k_low)
  | 0xF, x, 0x0, 0xa          -> LD_K (reg x)
  | 0xD, x, y, n              -> DRW (reg x, reg y, n)
  | 0x1, k_high, k_mid, k_low -> JP (merge_bits3 k_high k_mid k_low)
  | _ -> failwith "Unknown opcode encountered"
;;