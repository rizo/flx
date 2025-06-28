let () =
  Printexc.record_backtrace true;
  In_channel.fold_lines
    (fun () line ->
      match String.get line 0 with
      | exception _ -> print_newline ()
      | '#' -> print_endline line
      | ' ' -> ()
      | _ -> (
        let lex = Flx.Lexer.of_string line in
        try
          let flx = Flx.parse lex in
          Format.printf "%s@.    %a@." line Flx.pp_sexp flx
        with exn ->
          Format.printf "%s@.    error: %s@." line (Printexc.to_string exn)
      )
    )
    () stdin
