open Base

type register = int
type long_register = int ref

let reg byte = byte
let long_reg = ref 0 
(* let registers = Array.create ~len:16 0 *)


