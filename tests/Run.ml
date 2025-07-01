let () =
  Printexc.record_backtrace true;
  In_channel.fold_lines
    (fun () line ->
      match String.get line 0 with
      | exception _ -> print_newline ()
      | '#' -> print_endline line
      | ' ' -> ()
      | _ -> (
        let lex = Flx.Lex.read_string line in
        try
          let flx = Flx.parse lex in
          Format.printf "%s@.    %a@." line Flx.pp flx
        with Failure msg -> Format.printf "%s@.    error: %s@." line msg
      )
    )
    () stdin
