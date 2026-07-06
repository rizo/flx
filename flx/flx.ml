open Prelude

let rec parse_expr ?(rbp = 0) lex =
  let left = parse_prefix lex in
  parse_infix lex ~rbp left

and parse_prefix lex =
  let tok_sp = Lex.peek lex in
  match tok_sp.token with
  | Id id -> parse_atom lex (`id id)
  | Int int -> parse_atom lex (`int int)
  | Str str -> parse_atom lex (`str str)
  | Char c -> parse_atom lex (`char c)
  | Comment text -> parse_atom lex (`comment text)
  | Backtick -> parse_quote lex
  | Dollar -> parse_unquote lex
  | Template_start str -> parse_template ~start:str lex
  | Sym "@" -> parse_attr lex
  | Sym "|" as delim -> parse_sep_start lex ~delim (fun x -> `pipe x)
  | Sym (("~" | "!" | "#") as op) -> parse_prefix_op ~rbp:Precedence.juxt lex op
  | Sym op ->
    begin match tok_sp.sp with
    | `left -> parse_prefix_op ~rbp:Precedence.juxt lex op
    | _ -> parse_prefix_op ~rbp:(Precedence.juxt - 1) lex op
    end
  | Lparen -> parse_block lex Token.Rparen (fun x -> `parens x)
  | Lbrace -> parse_block lex Token.Rbrace (fun x -> `braces x)
  | Lbracket -> parse_block lex Token.Rbracket (fun x -> `brackets x)
  | Eof -> fail "%a: unexpected end of input" Lex.pp_loc (Lex.loc lex)
  | tok -> fail "%a: invalid token: %a" Lex.pp_loc (Lex.loc lex) Token.pp tok

and parse_infix lex ~rbp left =
  let tok_sp = Lex.peek lex in
  let tok = tok_sp.token in
  let precedence = Precedence.get_sp tok_sp in
  let lbp = abs precedence in
  let parse =
    let rbp = if precedence < 0 then lbp - 1 else lbp in
    match tok with
    | Eof | Rparen | Rbracket | Rbrace | Template_mid _ | Template_end _ ->
      fun _ -> assert false
    | Comma -> parse_sep_trailing lex ~delim:tok ~rbp (fun x -> `comma x)
    | Semi -> parse_sep_trailing lex ~delim:tok ~rbp (fun x -> `semi x)
    | Sym "." -> parse_sep lex ~delim:tok ~rbp (fun x -> `dot x)
    | Sym "|" -> parse_sep lex ~delim:tok ~rbp (fun x -> `pipe x)
    | Sym ("~" as op) ->
      begin match tok_sp.sp with
      | `left -> parse_seq ~rbp:Precedence.juxt lex
      | `right -> parse_postfix_op lex op
      | `both | `none -> parse_infix_op lex ~rbp op
      end
    | Sym "#" -> parse_seq ~rbp lex
    | Sym op -> parse_infix_op lex ~rbp op
    | _ -> parse_seq ~rbp lex
  in
  if lbp > rbp then
    let left' = parse left in
    parse_infix lex ~rbp left'
  else left

and parse_atom lex atom =
  Lex.advance lex;
  atom

and parse_quote lex =
  Lex.advance lex;
  let expr = parse_prefix lex in
  `quote expr

and parse_unquote lex =
  Lex.advance lex;
  let expr = parse_prefix lex in
  `unquote expr

and parse_attr lex =
  Lex.advance lex;
  let attr = parse_prefix lex in
  let { Token.token; _ } = Lex.peek lex in
  let precedence = abs (Precedence.get token) in
  let expr =
    if precedence <= Precedence.attr then None
    else Some (parse_expr ~rbp:Precedence.attr lex)
  in
  `attr (attr, expr)

and parse_template ~start lex0 =
  let lex = { lex0 with Lex.in_template = true } in
  Lex.advance lex;
  let rec loop acc =
    let { Token.token; _ } = Lex.peek lex in
    match token with
    | Template_mid str ->
      Lex.advance lex;
      loop (`str str :: acc)
    | Template_end str ->
      Lex.advance lex0;
      `str str :: acc
    | _ -> (
      let expr = parse_expr lex in
      let { Token.token; _ } = Lex.peek lex in
      match token with
      | Template_mid str ->
        Lex.advance lex;
        loop (`str str :: expr :: acc)
      | Template_end str ->
        Lex.advance lex0;
        `str str :: expr :: acc
      | unexpected ->
        fail "%a: invalid template syntax: %a" Lex.pp_loc (Lex.loc lex) Token.pp
          unexpected
    )
  in
  `template (List.rev (loop [ `str start ]))

and parse_seq lex ~rbp left =
  let rec loop acc =
    let tok_sp = Lex.peek lex in
    let tok_prec = Precedence.get_sp tok_sp in
    if tok_prec = Precedence.juxt then
      let expr = parse_expr ~rbp lex in
      loop (expr :: acc)
    else acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  `seq expr_list

and parse_postfix_op lex op left =
  Lex.advance lex;
  `postfix (op, left)

and parse_prefix_op ~rbp lex op =
  Lex.advance lex;
  let { Token.token; _ } = Lex.peek lex in
  match token with
  | Eof
  | Rparen
  | Rbrace
  | Rbracket
  | Template_mid _
  | Template_end _
  | Comma
  | Semi -> `op op
  | _tok ->
    let expr = parse_expr ~rbp lex in
    `prefix (op, expr)

and parse_infix_op lex op ~rbp left =
  Lex.advance lex;
  let { Token.token; _ } = Lex.peek lex in
  match token with
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
  Lex.advance lex;
  let precedence = Precedence.get delim in
  let lbp = abs precedence in
  let rbp = if precedence < 0 then lbp - 1 else lbp in
  let left = parse_expr ~rbp lex in
  parse_sep lex ~delim ~rbp mk left

and parse_sep lex ~delim ~rbp mk left =
  Lex.consume lex delim;
  let rec loop acc =
    let expr = parse_expr ~rbp lex in
    let { Token.token; _ } = Lex.peek lex in
    if Token.eq token delim then (
      Lex.advance lex;
      loop (expr :: acc)
    )
    else expr :: acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  mk expr_list

and parse_sep_trailing lex ~delim ~rbp mk left =
  Lex.consume lex delim;
  let rec loop acc =
    let { Token.token; _ } = Lex.peek lex in
    match token with
    | Rparen | Rbrace | Rbracket | Eof -> acc
    | _ ->
      let expr = parse_expr ~rbp lex in
      let tok_sp = Lex.peek lex in
      let tok = tok_sp.token in
      if Token.eq tok delim then (
        Lex.advance lex;
        loop (expr :: acc)
      )
      else expr :: acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  mk expr_list

and parse_block lex closing mk =
  let lex' = { lex with in_template = false } in
  Lex.advance lex';
  let { Token.token; _ } = Lex.peek lex in
  if Token.eq token closing then (
    Lex.consume lex closing;
    mk (`seq [])
  )
  else
    let expr = parse_expr lex' in
    Lex.consume lex closing;
    mk expr

let parse lex =
  let expr = parse_expr ~rbp:0 lex in
  let { Token.token; _ } = Lex.peek lex in
  if Token.eq token Eof then expr
  else fail "%a: unexpected token: %a" Lex.pp_loc (Lex.loc lex) Token.pp token

module Lex = Lex

let pp = Expr.pp

type t = Expr.t
