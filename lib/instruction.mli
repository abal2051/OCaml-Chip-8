open Base

type instruction

val decode: (int * int * int * int) -> instruction

val to_string: instruction -> string