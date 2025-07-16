module Fmt = struct
  let pf = Format.fprintf
  let pr = Format.printf
  let epr = Format.eprintf
  let list ?sep pp l = Format.pp_print_list ?pp_sep:sep pp l
  let sp = Format.pp_print_space
end

let print ?(break = Format.pp_print_newline) fmt =
  Format.kfprintf (fun f -> break f ()) Format.std_formatter fmt

let todo () = failwith "TODO"

let wip what flx =
  Format.eprintf "WIP: %s: %a@." what Flx.pp flx;
  exit 42
