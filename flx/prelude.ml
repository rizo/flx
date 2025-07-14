let debug ?(break = Format.pp_print_newline) fmt =
  Format.kfprintf (fun f -> break f ()) Format.std_formatter fmt

let fail fmt = Format.kasprintf failwith fmt
let ( = ) : int -> int -> bool = ( = )

let tap f x =
  f x;
  x

module Fmt = struct
  let pf = Format.fprintf
  let pr = Format.printf
  let list ?sep pp l = Format.pp_print_list ?pp_sep:sep pp l
  let sp = Format.pp_print_space
end
