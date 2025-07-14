{
  type t = {
    lexbuf : Lexing.lexbuf;
    strbuf : Buffer.t;
    token : Token.t ref;
    in_template : bool;
  }

  let update_loc lexbuf file line absolute chars =
    let pos = lexbuf.Lexing.lex_curr_p in
    let new_file = match file with
                   | None -> pos.pos_fname
                   | Some s -> s
    in
    lexbuf.Lexing.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    } 

  type loc = { f_name : string; l_start : int; c_start : int; l_end : int; c_end : int }

  let loc {lexbuf;_} = {
    f_name = lexbuf.Lexing.lex_start_p.pos_fname;
    l_start = lexbuf.Lexing.lex_start_p.pos_lnum;
    c_start = lexbuf.Lexing.lex_start_p.pos_cnum;
    l_end = lexbuf.Lexing.lex_curr_p.pos_lnum;
    c_end = lexbuf.Lexing.lex_curr_p.pos_cnum;
  }

  let pp_loc f loc =
    let fname = if String.length loc.f_name = 0 then "<input>" else loc.f_name in
    Format.fprintf f "%s: line %d: col %d" fname loc.l_start loc.c_start

  let flush_buffer buf =
    let str = Buffer.contents buf in
    Buffer.reset buf;
    str
}

(* Ident *)
let ident_lower_char = ['a'-'z' '_']
let ident_upper_char = ['A'-'Z']
let ident_inner_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let ident_upper = (ident_upper_char) ident_inner_char*
let ident_lower = (ident_lower_char) ident_inner_char*

let op_char =
  ['!' '$' '%' '#' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let op = op_char+

let digit = ['0'-'9']
let nonzero = ['1'-'9']
let int = (digit | nonzero digit+)

rule read lex = parse
  (* Whitespace *)
  | [' ' '\t']+ { read lex lexbuf }

  (* Whitespace: update loc info *)
  | '\n' | '\r' {
    update_loc lexbuf None 1 false 0;
    read lex lexbuf
  }

  (* Line comment *)
  | "//"[^'\n']* { read lex lexbuf }

  (* Symbols *)
  | '`' { Token.Backtick }
  | ',' { Token.Comma }
  | ';' { Token.Semi }
  | '\\' { Token.Sym ("\\") }
  | '(' { Token.Lparen }
  | ')' { Token.Rparen }
  | '[' { Token.Lbracket }
  | ']' { Token.Rbracket }
  | '{' {
    Token.Lbrace
  }

  (* Rbrace or string tepmlate *)
  | '}' {
    if lex.in_template then
      read_string true lex lexbuf
    else Token.Rbrace
  }

  (* String *)
  | '"' {
    read_string false lex lexbuf
  }

  (* Char *)
  | '\'' ([^ '\''] as x) '\'' { Token.Char x }

  (* Identifiers *)
  | ident_lower { Token.Id (Lexing.lexeme lexbuf) }
  | ident_upper { Token.Id (Lexing.lexeme lexbuf) }

  (* Operators *)
  | op { Token.Sym (Lexing.lexeme lexbuf) }

  (* Integers *)
  | int { Token.Int (int_of_string (Lexing.lexeme lexbuf)) }

  | eof { Eof }

  | _ {
    let err = Format.asprintf "%a: invalid input: %S"
      pp_loc (loc lex)
      (Lexing.lexeme lexbuf) in
    failwith err
  }


and read_string is_template lex = parse
  (* End of string *)
  | '"'  {
    let str = flush_buffer lex.strbuf in
    (* Prelude.debug "is_template=%b in_template=%b" is_template lex.in_template; *)
    if is_template then (
      Template_end str
    )
    else Str str
  }
  (* Escape sequences *)
  | '\\' ('\\' | '\'' | '"' | ' ' | '$' as c) {
    Buffer.add_char lex.strbuf c;
    read_string is_template lex lexbuf
  }
  | "\\n" {
    Buffer.add_char lex.strbuf '\n';
    read_string is_template lex lexbuf
  }
  | "\\r" {
    Buffer.add_char lex.strbuf '\r';
    read_string is_template lex lexbuf
  }
  | "\\t" {
    Buffer.add_char lex.strbuf '\t';
    read_string is_template lex lexbuf
  }
  | "\\b" {
    Buffer.add_char lex.strbuf '\b';
    read_string is_template lex lexbuf
  }
  | "\\" _ as x {
    let err = Format.asprintf "%a: invalid escape sequence %S, must be one of:  \\ \" \n \\$"
      pp_loc (loc lex) x in
    failwith err
  }
  | "${" {
    let str = flush_buffer lex.strbuf in
    (* Prelude.debug "is_template=%b in_template=%b" is_template lex.in_template; *)
    if is_template then
      Template_mid str
    else (
      Template_start str
    )
  }
  | '$' {
    Buffer.add_char lex.strbuf '$';
    read_string is_template lex lexbuf
  }
  | [^ '"' '\\' '$']+ {
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes lex.strbuf lexbuf.lex_buffer lexbuf.lex_start_pos len;
    read_string is_template lex lexbuf
  }
  | eof {
    let err = Format.asprintf "%a: unterminated %s"
      pp_loc (loc lex)
      (if is_template then "template string" else "string literal") in
    failwith err
  }

{
  let read_lexbuf lexbuf =
    let lex = {
      lexbuf;
      token = ref Token.Eof;
      strbuf = Buffer.create 64;
      in_template = false;
    } in
    lex.token := read lex lexbuf;
    lex

  let read_string s =
    let lexbuf = Lexing.from_string s in
    read_lexbuf lexbuf

  let read_channel ?file_name ic =
    let lexbuf = Lexing.from_channel ic in
    (match file_name with Some f -> Lexing.set_filename lexbuf f | _ -> ());
    read_lexbuf lexbuf

  let next lex =
    lex.token := read lex lex.lexbuf

  let peek lex =
    !(lex.token)

  let consume lex expected =
    let tok = peek lex in
    if Token.eq tok expected then next lex
    else
      if Token.eq tok Token.Eof then
        let err = Format.asprintf "%a: end of input when expecting %a" pp_loc (loc lex) Token.pp expected in
        failwith err
      else
        let err = Format.asprintf "%a: expected %a, got %a" pp_loc (loc lex) Token.pp expected Token.pp tok in
        failwith err
 }
