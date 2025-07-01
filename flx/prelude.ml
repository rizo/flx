let debug ?(break = Format.pp_print_newline) fmt =
  Format.kfprintf (fun f -> break f ()) Format.std_formatter fmt

let fail fmt = Format.kasprintf failwith fmt
let ( = ) : int -> int -> bool = ( = )
