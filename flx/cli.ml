let print_usage () = Format.eprintf "usage: flx [input_file]"

let () =
  Printexc.record_backtrace true;
  let lex =
    match Sys.argv with
    | [| _; "-" |] | [| _ |] -> Flx.Lex.read_channel stdin
    | [| _; file_name |] -> Flx.Lex.read_channel ~file_name (open_in file_name)
    | _ ->
      print_usage ();
      exit 1
  in
  try
    let flx = Flx.parse lex in
    Format.printf "%a@." Flx.pp flx
  with Failure msg -> Format.printf "error: %s@." msg
