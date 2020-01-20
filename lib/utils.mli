open Base 


val lower_bits: int -> int 
val upper_bits: int -> int 
val make_byte_seq: int -> int -> (int * int * int * int)
val process_bytes: int list -> (int * int * int * int) list
val merge_bits: int -> int -> int
val merge_bits3: int -> int -> int -> int