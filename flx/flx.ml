open Prelude

let rec parse_expr ?(rbp = 0) lex =
  let left = parse_prefix lex in
  parse_infix lex ~rbp left

and parse_prefix lex =
  match Lex.peek lex with
  | Id id -> parse_atom lex (`id id)
  | Int int -> parse_atom lex (`int int)
  | Str str -> parse_atom lex (`str str)
  | Char c -> parse_atom lex (`char c)
  | Template_start str -> parse_template ~start:str lex
  | Sym "|" as tok -> parse_sep_start lex ~delim:tok (fun x -> `pipe x)
  | Sym op -> parse_prefix_op lex op
  | Lparen -> parse_block lex Token.Rparen (fun x -> `parens x)
  | Lbrace -> parse_block lex Token.Rbrace (fun x -> `braces x)
  | Lbracket -> parse_block lex Token.Rbracket (fun x -> `brackets x)
  | Eof -> fail "%a: unexpected end of input" Lex.pp_loc (Lex.loc lex)
  | tok -> fail "%a: invalid token: %a" Lex.pp_loc (Lex.loc lex) Token.pp tok

and parse_infix lex ~rbp left =
  let tok = Lex.peek lex in
  let precedence = Precedence.get tok in
  let lbp = abs precedence in
  let parse =
    let rbp = if precedence < 0 then lbp - 1 else lbp in
    match tok with
    | Eof | Rparen | Rbracket | Rbrace | Template_mid _ | Template_end _ ->
      fun _ -> assert false
    | Comma -> parse_sep_end lex ~delim:tok ~rbp (fun x -> `comma x)
    | Semi -> parse_sep_end lex ~delim:tok ~rbp (fun x -> `semi x)
    | Sym "." -> parse_sep lex ~delim:tok ~rbp (fun x -> `dot x)
    | Sym "|" -> parse_sep lex ~delim:tok ~rbp (fun x -> `pipe x)
    | Sym op -> parse_infix_op lex ~rbp op
    (* TODO ensure tpl toks are not here *)
    | _ -> parse_seq ~rbp lex
  in
  if lbp > rbp then
    let left' = parse left in
    parse_infix lex ~rbp left'
  else left

and parse_atom lex atom =
  Lex.next lex;
  atom

and parse_template ~start lex0 =
  let lex = { lex0 with Lex.in_template = true } in
  Lex.next lex;
  let rec loop acc =
    let expr = parse_expr lex in
    match Lex.peek lex with
    | Template_mid str ->
      Lex.next lex;
      loop (`str str :: expr :: acc)
    | Template_end str ->
      Lex.next lex0;
      `str str :: expr :: acc
    | unexpected ->
      fail "%a: invalid template syntax: %a" Lex.pp_loc (Lex.loc lex) Token.pp
        unexpected Expr.pp expr
  in
  match Lex.peek lex with
  | Template_end end_str ->
    Lex.next lex0;
    `template [ `str start; `str end_str ]
  | Template_mid mid_str ->
    Lex.next lex;
    let tpl = List.rev (loop [ `str mid_str; `str start ]) in
    `template tpl
  | _ ->
    let tpl = List.rev (loop [ `str start ]) in
    `template tpl

and parse_seq lex ~rbp left =
  let rec loop acc =
    let tok = Lex.peek lex in
    let tok_prec = Precedence.get tok in
    if tok_prec = Precedence.juxt then
      let expr = parse_expr ~rbp lex in
      loop (expr :: acc)
    else acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  `seq expr_list

and parse_prefix_op lex op =
  Lex.next lex;
  match Lex.peek lex with
  | Eof
  | Rparen
  | Rbrace
  | Rbracket
  | Template_mid _
  | Template_end _
  | Comma
  | Semi -> `op op
  | _ ->
    let expr = parse_expr lex in
    `prefix (op, expr)

and parse_infix_op lex op ~rbp left =
  Lex.next lex;
  match Lex.peek lex with
  | Eof
  | Rparen
  | Rbrace
  | Rbracket
  | Template_mid _
  | Template_end _
  | Comma
  | Semi -> `postfix (op, left)
  | _ ->
    let right = parse_expr ~rbp lex in
    `infix (op, left, right)

and parse_sep_start lex ~delim mk =
  Lex.next lex;
  let precedence = Precedence.get delim in
  let lbp = abs precedence in
  let rbp = if precedence < 0 then lbp - 1 else lbp in
  let left = parse_prefix lex in
  parse_sep lex ~delim ~rbp mk left

and parse_sep lex ~delim ~rbp mk left =
  Lex.consume lex delim;
  let rec loop acc =
    let expr = parse_expr ~rbp lex in
    let tok = Lex.peek lex in
    if Token.eq tok delim then (
      Lex.next lex;
      loop (expr :: acc)
    )
    else expr :: acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  mk expr_list

and parse_sep_end lex ~delim ~rbp mk left =
  Lex.consume lex delim;
  let rec loop acc =
    match Lex.peek lex with
    | Rparen | Rbrace | Rbracket | Eof -> acc
    | _ ->
      let expr = parse_expr ~rbp lex in
      let tok = Lex.peek lex in
      if Token.eq tok delim then (
        Lex.next lex;
        loop (expr :: acc)
      )
      else expr :: acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  mk expr_list

and parse_block lex closing mk =
  let lex' = { lex with in_template = false } in
  Lex.next lex';
  let tok = Lex.peek lex in
  if Token.eq tok closing then (
    Lex.consume lex closing;
    mk (`seq [])
  )
  else
    let expr = parse_expr lex' in
    Lex.consume lex closing;
    mk expr

let parse lex =
  let expr = parse_expr ~rbp:0 lex in
  let tok = Lex.peek lex in
  if Token.eq tok Eof then expr
  else fail "%a: unexpected token: %a" Lex.pp_loc (Lex.loc lex) Token.pp tok

module Lex = Lex

let pp = Expr.pp
