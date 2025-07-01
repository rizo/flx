{
  type t = {
    lexbuf : Lexing.lexbuf;
    strbuf : Buffer.t;
    mutable token : Token.t;
    mutable line_start : int;
    mutable line_count : int;
    mutable template_level : int;
    mutable is_template : bool;
  }

  let update_loc_ lexer =
    lexer.line_count <- lexer.line_count + 1;
    lexer.line_start <- lexer.lexbuf.lex_abs_pos + lexer.lexbuf.lex_curr_pos

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

rule read lexer = parse
  (* Whitespace *)
  | [' ' '\t']+ { read lexer lexbuf }

  (* Whitespace: update loc info *)
  | '\n' | '\r' {
    update_loc lexbuf None 1 false 0;
    read lexer lexbuf
  }

  (* Line comment *)
  | "//"[^'\n']* { read lexer lexbuf }

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
    if lexer.is_template then (
      lexer.template_level <- lexer.template_level - 1;
    );
    Token.Lbrace
  }

  (* Rbrace or string tepmlate *)
  | '}' {
    if lexer.template_level > 0 then (
        lexer.is_template <- true;
        lexer.template_level <- lexer.template_level - 1;
        read_string lexer lexbuf
    )
    else (
      if lexer.is_template then (
        lexer.template_level <- lexer.template_level + 1
      );

      Token.Rbrace
    )
  }

  (* String *)
  | '"' {
    (* Prelude.debug "lexer.is_template=%b lexer.template_level=%d" lexer.is_template lexer.template_level; *)
    lexer.is_template <- false;
    read_string lexer lexbuf
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
    Fmt.failwith "%a: invalid input: %S"
      pp_loc (loc lexer)
      (Lexing.lexeme lexbuf)
  }


and read_string lexer = parse
  (* End of string *)
  | '"'  {
    let str = Buffer.contents lexer.strbuf in
    Buffer.reset lexer.strbuf;
    if lexer.is_template then (
      lexer.is_template <- false;
      Template_end str
    )
    else (
      Str str
    )
  }
  (* Escape sequences *)
  | '\\' ('\\' | '\'' | '"' | ' ' | '$' as c) {
    Buffer.add_char lexer.strbuf c;
    read_string lexer lexbuf
  }
  | "\\n" {
    Buffer.add_char lexer.strbuf '\n';
    read_string lexer lexbuf
  }
  | "\\r" {
    Buffer.add_char lexer.strbuf '\r';
    read_string lexer lexbuf
  }
  | "\\t" {
    Buffer.add_char lexer.strbuf '\t';
    read_string lexer lexbuf
  }
  | "\\b" {
    Buffer.add_char lexer.strbuf '\b';
    read_string lexer lexbuf
  }
  | "\\" _ as x {
    Fmt.failwith "%a: invalid escape sequence %S, must be one of:  \\ \" \n \\$"
      pp_loc (loc lexer) x
  }
  | "${" {
    let str = Buffer.contents lexer.strbuf in
    Buffer.reset lexer.strbuf;
    lexer.template_level <- lexer.template_level + 1;
    if lexer.is_template then
      Template_mid str
    else (
      lexer.is_template <- true;
      Template_start str
    )
  }
  | '$' {
    Buffer.add_char lexer.strbuf '$';
    read_string lexer lexbuf
  }
  | [^ '"' '\\' '$']+ {
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes lexer.strbuf lexbuf.lex_buffer lexbuf.lex_start_pos len;
    read_string lexer lexbuf
  }
  | eof {
    Fmt.failwith "%a: unterminated %s"
      pp_loc (loc lexer)
      (if lexer.is_template then "template string" else "string literal")
  }

{
  let read_lexbuf lexbuf =
    let lexer = {
      line_count = 1;
      line_start = 0;
      lexbuf;
      token = Eof;
      strbuf = Buffer.create 64;
      template_level = 0;
      is_template = false;
    } in
    lexer.token <- read lexer lexbuf;
    lexer

  let read_string s =
    let lexbuf = Lexing.from_string s in
    read_lexbuf lexbuf

  let read_channel ?file_name ic =
    let lexbuf = Lexing.from_channel ic in
    (match file_name with Some f -> Lexing.set_filename lexbuf f | _ -> ());
    read_lexbuf lexbuf

  let move lexer =
    lexer.token <- read lexer lexer.lexbuf
    (* ; Prelude.debug "tok=%a" Token.pp lexer.token *)


  let next lexer =
    move lexer;
    lexer.token

  let peek lexer =
    lexer.token

  let consume lex expected =
    let tok = peek lex in
    if Token.eq tok expected then move lex
    else
      if Token.eq tok Token.Eof then
        Fmt.failwith "%a: end of input when expecting %a" pp_loc (loc lex) Token.pp expected
      else
        Fmt.failwith "%a: expected %a, got %a" pp_loc (loc lex) Token.pp expected Token.pp tok

  let line_number lexer =
    lexer.line_count
 }
