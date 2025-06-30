(*
 - available
 - requested
 - supply
 - demand

 *)

open struct
  type 'a iter = ('a -> unit) -> unit
  type 'a fmt = Format.formatter -> 'a -> unit

  let debug ?(break : unit fmt = Format.pp_print_newline) fmt =
    Format.kfprintf (fun f -> break f ()) Format.std_formatter fmt

  let string_of_char = String.make 1
  let fail fmt = Format.kasprintf failwith fmt
  let fmt = Format.asprintf
  let ( = ) : int -> int -> bool = ( = )
  let ( <> ) : int -> int -> bool = ( <> )
end

module type Reader = sig
  type t

  val request : t -> int -> int

  (** If the character cannot be read or dropped, raises Empty. *)

  val drop : t -> int -> unit
  (** Drops multiple characters from the reader. *)

  val peek : t -> char
  (** Peek a single character without consuming it. *)

  val next : t -> char
  (** Peek a single character consuming it. *)
end

module Reader = struct
  exception Empty

  type t = { string : string; mutable offset : int }

  let of_string string = { string; offset = 0 }
  let available t = String.length t.string - t.offset
  let request t _n = available t

  let drop t n =
    if n = 0 then ()
    else if n < 0 then invalid_arg "drop"
    else t.offset <- min (t.offset + n) (String.length t.string)

  let is_empty t = String.length t.string = t.offset
  let peek t = if is_empty t then raise Empty else String.get t.string t.offset

  let next t =
    let c = peek t in
    drop t 1;
    c

  let dump t =
    debug "r.offset=%d r.len=%d r.available=%d" t.offset
      (String.length t.string) (available t)
end

type token =
  | Id of string
  | Sym of string
  | Int of int
  | Str of string
  | Char of char
  | Backtick
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Comma
  | Semi
  | Template_start of string
  | Template_mid of string
  | Template_end of string
  | Eof

module Token = struct
  type t = token

  let pp =
    let pf = Format.fprintf in
    fun f token ->
      match token with
      | Id x -> pf f "%s" x
      | Lparen -> pf f "("
      | Rparen -> pf f ")"
      | Lbrace -> pf f "{"
      | Rbrace -> pf f "}"
      | Lbracket -> pf f "["
      | Rbracket -> pf f "]"
      | Backtick -> pf f "`"
      | Comma -> pf f ","
      | Semi -> pf f ";"
      | Str x -> pf f "%S" x
      | Char x -> pf f "%c" x
      | Int x -> pf f "%d" x
      | Sym x -> pf f "%s" x
      | Template_start x -> pf f "(template-start %S)" x
      | Template_mid x -> pf f "(template-mid %S)" x
      | Template_end x -> pf f "(template-end %S)" x
      | Eof -> pf f "(eof)"

  let pp f t = Fmt.pf f "`%a`" pp t
  let eq t1 t2 = Stdlib.( = ) t1 t2
end

(** Operator precedence of the tokens.

    Higher numbers represent higher precedence. For exmaple, [*] has higher
    precedence than [+], which results in the expression [a + b * c] being
    parsed as [a + (b * c)].

    The sign of the number controls associativity. Positive numbers have
    left-to-right associativity, like in [(a + b) + c], while negative numbers
    represent right-to-left associativity, like in [a = (b = c)].

    Precedence value [0] has a special meaning: it denotes terminator tokens.
    When a terminator token is encountered, the current expression stops, bigin
    control to the parent parser.

    Juxtaposition of tokens has a very high precedence. In a way, juxtaposition
    is like having an invisible operator between [f b]. Since juxtaposition has
    a high precedence, expressions such as [x = f a + 1] are parsed as
    [x = ((f a) + 1)].

    The highest precedence is resreved for the [.] operator token, which is even
    higher than juxtaposition. This ensures that expressions such as [f a.b + 1]
    are parsed as [(f (a.b)) + 1].

    SEE: https://ocaml.org/manual/5.3/api/Ocaml_operators.html *)
module Power = struct
  let juxt = 200

  let get (tok : token) =
    match tok with
    (* Terminators *)
    | Eof | Rparen | Rbrace | Rbracket | Template_mid _ | Template_end _ -> 0
    | Semi -> 10
    | Comma -> 20
    | Sym "=" -> -30
    | Sym "|" -> 40
    | Sym ":" -> -50
    | Sym "::" -> 55
    | Sym "->" -> -60
    | Sym "!" -> 60
    | Sym ":=" -> -60
    | Sym "<-" -> -60
    | Sym ("&" | "&&") -> -70
    | Sym "||" -> -70
    | Sym "**" -> -80
    | Sym "." -> 300
    | Sym op -> (
      match op.[0] with
      | '@' -> 100
      | '=' -> 101
      | '<' | '>' -> 102
      | '#' | '&' -> 102
      | '|' -> 102
      | '+' | '-' -> 103
      | '*' | '/' -> 104
      | _ -> 100
    )
    (* Juxtaposition *)
    | Id _
    | Str _
    | Char _
    | Int _
    | Backtick
    | Lparen
    | Lbrace
    | Lbracket
    | Template_start _ -> juxt
end

let is_op_char c =
  match c with
  | '!'
  | '$'
  | '%'
  | '#'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '|'
  | '~' -> true
  | _ -> false

exception Unexpected of string

let unexpected x = raise (Unexpected x)

let rec pp_sexp f t =
  match t with
  | `id x -> Fmt.pf f "%s" x
  | `op x -> Fmt.pf f "%s" x
  | `int x -> Fmt.pf f "%d" x
  | `str x -> Fmt.pf f "%S" x
  | `char x -> Fmt.pf f "%C" x
  | `parens x -> Fmt.pf f "@[<hv2>((_)@ %a@])" pp_sexp x
  | `brackets x -> Fmt.pf f "@[<hv2>([_]@ %a@])" pp_sexp x
  | `braces x -> Fmt.pf f "@[<hv2>({_}@ %a@])" pp_sexp x
  | `prefix (fix, x) -> Fmt.pf f "@[<hv2>(%s_@ %a)@]" fix pp_sexp x
  | `infix (fix, x, y) ->
    Fmt.pf f "@[<hv2>(_%s_@ %a@ %a)@]" fix pp_sexp x pp_sexp y
  | `postfix (fix, x) -> Fmt.pf f "@[<hv2>(_%s@ %a)@]" fix pp_sexp x
  | `comma [] -> Fmt.pf f "(,)"
  | `comma xs -> Fmt.pf f "(, @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
  | `dot xs -> Fmt.pf f "(. @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
  | `pipe xs -> Fmt.pf f "(| @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
  | `semi [] -> Fmt.pf f "(;)"
  | `semi xs -> Fmt.pf f "(; @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
  | `seq [] -> Fmt.pf f "()"
  | `quote x -> Fmt.pf f "`(%a)" pp_sexp x
  | `seq xs -> Fmt.pf f "(_ @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
  | `template [] -> Fmt.pf f "($)"
  | `template xs -> Fmt.pf f "($ @[<hv2>%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs

type lexer = {
  reader : Reader.t;
  mutable token : Token.t option;
  strbuf : Buffer.t;
  mutable template_level : int option;
}

let rec read lex : Token.t =
  let available = Reader.request lex.reader 1 in
  if available = 0 then Eof
  else
    match Reader.next lex.reader with
    | ' ' -> read lex
    | ('a' | 'b' | 'c' | 'd') as x -> Id (string_of_char x)
    | ('1' | '2' | '3' | '4') as x -> Int (Char.code x - 48)
    | ',' -> Comma
    | ';' -> Semi
    | '(' -> Lparen
    | '[' -> Lbracket
    | '{' -> Lbrace
    | ')' -> Rparen
    | ']' -> Rbracket
    | '"' -> read_string lex
    | '}' when Option.is_some lex.template_level ->
      decr_template_level lex;
      read_string lex
    | '}' -> Rbrace
    | op0 when is_op_char op0 ->
      let buf = Buffer.create 4 in
      Buffer.add_char buf op0;
      let rec loop () =
        let available = Reader.request lex.reader 1 in
        if available >= 1 then
          let c = Reader.peek lex.reader in
          if is_op_char c then (
            Reader.drop lex.reader 1;
            Buffer.add_char buf c;
            loop ()
          )
      in
      loop ();
      let op = Buffer.contents buf in
      Sym op
    | c -> unexpected (string_of_char c)

and read_string lex =
  let available = Reader.request lex.reader 1 in
  if available = 0 then fail "error: unterminated string"
  else
    match Reader.next lex.reader with
    (* End of string *)
    | '"' ->
      let str = Buffer.contents lex.strbuf in
      Buffer.reset lex.strbuf;
      begin
        match lex.template_level with
        | None -> Str str
        | Some 0 ->
          lex.template_level <- None;
          Template_end str
        | Some l ->
          fail "unbalanced template block: %d off=%d" l lex.reader.offset
      end
    (* Escape sequences *)
    | '\\' -> begin
      let available = Reader.request lex.reader 1 in
      if available = 0 then fail "error: missing escaped character after '\'"
      else
        match Reader.next lex.reader with
        | ('"' | '\'' | ' ' | '\\' | '$') as c ->
          Buffer.add_char lex.strbuf c;
          read_string lex
        | 'n' ->
          Buffer.add_char lex.strbuf '\n';
          read_string lex
        | 'r' ->
          Buffer.add_char lex.strbuf '\r';
          read_string lex
        | 't' ->
          Buffer.add_char lex.strbuf '\t';
          read_string lex
        | 'b' ->
          Buffer.add_char lex.strbuf '\b';
          read_string lex
        | unknown -> fail "error: unknown escape sequence: \\%c" unknown
    end
    (* Template *)
    | '$' -> begin
      let available = Reader.request lex.reader 1 in
      if available = 0 then fail "error: unterminated string after '$'"
      else
        match Reader.next lex.reader with
        | '{' ->
          let is_template_start = Option.is_none lex.template_level in
          incr_template_level lex;
          let str = Buffer.contents lex.strbuf in
          Buffer.reset lex.strbuf;
          if is_template_start then Template_start str else Template_mid str
        | c ->
          Buffer.add_char lex.strbuf '$';
          Buffer.add_char lex.strbuf c;
          read_string lex
    end
    (* String char *)
    | c ->
      Buffer.add_char lex.strbuf c;
      read_string lex

and incr_template_level lex =
  match lex.template_level with
  | None -> lex.template_level <- Some 1
  | Some n -> lex.template_level <- Some (n + 1)

and decr_template_level lex =
  match lex.template_level with
  | None -> fail "error: decr_template_level: template_level is None"
  | Some n -> lex.template_level <- Some (n - 1)

module Lexer = struct
  let of_string str =
    let reader = Reader.of_string str in
    let strbuf = Buffer.create 64 in
    { reader; token = None; strbuf; template_level = None }
end

let advance lex = lex.token <- Some (read lex)

let peek lex =
  let tok =
    match lex.token with
    | Some tok -> tok
    | None ->
      let tok = read lex in
      lex.token <- Some tok;
      tok
  in
  (* debug "peek: tok=%a" Token.pp tok; *)
  tok

let consume lex expected =
  let tok = peek lex in
  if Token.eq tok expected then advance lex
  else fail "consume: expected %a, actual %a" Token.pp expected Token.pp tok

let is_empty lex = Reader.is_empty lex.reader

let rec parse_expr ?(rbp = 0) lex =
  let left = parse_prefix lex in
  let expr = parse_infix lex ~rbp left in
  expr

and parse_prefix lex =
  let tok = peek lex in
  match peek lex with
  | Id id ->
    advance lex;
    `id id
  | Int int ->
    advance lex;
    `int int
  | Str str ->
    advance lex;
    `str str
  | Template_start str -> parse_template ~start:str lex
  | Sym op -> parse_prefix_op lex op
  | Lparen -> parse_block lex tok Rparen (fun x -> `parens x)
  | Lbrace -> parse_block lex tok Rbrace (fun x -> `braces x)
  | Lbracket -> parse_block lex tok Rbracket (fun x -> `brackets x)
  | Eof -> fail "start: unexpected eof"
  | tok -> fail "start: unexpected token: %a" Token.pp tok

and parse_infix lex ~rbp left =
  let tok = peek lex in
  let power = Power.get tok in
  let lbp = abs power in
  let power = if power < 0 then lbp - 1 else lbp in
  let parse =
    match tok with
    | Eof | Rparen | Rbracket | Rbrace -> Fun.id
    | Comma ->
      parse_seq_sep lex ~delim:tok ~trailing:true ~power (fun x -> `comma x)
    | Semi ->
      parse_seq_sep lex ~delim:tok ~trailing:true ~power (fun x -> `semi x)
    | Sym "." ->
      parse_seq_sep lex ~delim:tok ~trailing:false ~power (fun x -> `dot x)
    | Sym "|" ->
      parse_seq_sep lex ~trailing:false ~delim:tok ~power (fun x -> `pipe x)
    | Sym op -> parse_infix_op lex ~power op
    | _ -> parse_seq ~rbp:power lex
  in
  if lbp > rbp then
    let left' = parse left in
    parse_infix lex ~rbp left'
  else left

and parse_template ~start lex =
  advance lex;
  let rec loop acc =
    let expr = parse_expr lex in
    match peek lex with
    | Template_mid str ->
      advance lex;
      loop (`str str :: expr :: acc)
    | Template_end str ->
      advance lex;
      `str str :: expr :: acc
    | _ -> fail "expected end of template"
  in
  let tpl = List.rev (loop [ `str start ]) in
  `template tpl

and parse_seq lex ~rbp left =
  let rec loop acc =
    let tok = peek lex in
    let tok_power = Power.get tok in
    if tok_power = Power.juxt then
      let expr = parse_expr ~rbp lex in
      loop (expr :: acc)
    else acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  `seq expr_list

and parse_prefix_op lex op =
  advance lex;
  let expr = parse_expr lex in
  `prefix (op, expr)

and parse_infix_op lex op ~power left =
  advance lex;
  match peek lex with
  | Eof | Rparen | Rbrace | Rbracket | Comma | Semi -> `postfix (op, left)
  | _ ->
    let right = parse_expr ~rbp:power lex in
    `infix (op, left, right)

and parse_seq_sep ~trailing lex ~delim ~power mk left =
  consume lex delim;
  let rec loop acc =
    match peek lex with
    (* Trailing position handling *)
    | (Rparen | Rbrace | Rbracket | Eof) when trailing -> acc
    | _ ->
      let expr = parse_expr ~rbp:power lex in
      let tok = peek lex in
      if Token.eq tok delim then (
        advance lex;
        loop (expr :: acc)
      )
      else expr :: acc
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  mk expr_list

and parse_block lex opening closing mk =
  consume lex opening;
  let expr = parse_expr lex in
  consume lex closing;
  mk expr

let parse lex =
  let expr = parse_expr ~rbp:0 lex in
  if Token.eq (peek lex) Eof then expr
  else fail "parsing stopped at: %a" Token.pp (peek lex)
