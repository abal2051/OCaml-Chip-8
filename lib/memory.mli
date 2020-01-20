open Base

type register
type long_register

val reg: int -> register
val reg_index: register -> int
val long_reg : long_register
