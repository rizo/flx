let () =
  Printexc.record_backtrace true;
  let str = In_channel.input_all stdin in
  let lex = Flx.Lexer.of_string str in
  let flx = Flx.parse lex in
  Format.printf "%a@." Flx.pp_sexp flx
