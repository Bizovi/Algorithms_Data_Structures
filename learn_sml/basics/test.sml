use "syntax.sml"; (* Important! use ; when importing a module *)

(* Fiddling around in the REPL *)

(* Some extremely basic test cases on the functions *)

val test1 = double 17 = 34
val test2 = double 0 = 0
val test3 = triple ~4 = ~12
val test4 = triple 0 = 0
val test5 = customMult(12,27) = 324
val test6 = customMult(~12,27) = ~324