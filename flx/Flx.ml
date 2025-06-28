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

(*
  Operator precedence, also known as binding power, is defined as a number.

  Higher numbers represent higher binding power.

  The sign of the number controls associativity. Positive numbers have
  left-to-right associativity, while negative numbers represent right-to-left
  associativity. 

  SEE: https://ocaml.org/manual/5.3/api/Ocaml_operators.html
*)
module Power = struct
  let semi = 10
  let comma = 20
  let colon = 25

  let get str =
    match str with
    | "=" -> -30
    | "|" -> -40
    | ":" -> -50
    | "::" -> 55
    | "->" -> -60
    | "!" -> 60
    | ":=" -> -60
    | "<-" -> -60
    | "&" | "&&" -> -70
    | "||" -> -70
    | "**" -> -80
    | "." -> 310
    | _ -> (
      match str.[0] with
      | '@' -> 100
      | '=' -> 101
      | '<' | '>' -> 102
      | '#' | '&' -> 102
      | '|' -> 102
      | '+' | '-' -> 103
      | '*' | '/' -> 104
      | _ -> 100
    )

  let juxt = 300
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
  | `semi [] -> Fmt.pf f "(;)"
  | `semi xs -> Fmt.pf f "(; @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
  | `seq [] -> Fmt.pf f "()"
  | `quote x -> Fmt.pf f "`(%a)" pp_sexp x
  | `seq xs -> Fmt.pf f "(_ @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
  | `template [] -> Fmt.pf f "($)"
  | `template xs -> Fmt.pf f "($ @[<hv2>%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs

type token =
  | Id of string
  | Op of string
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
  | Template_str of string
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
      | Op x -> pf f "%s" x
      | Template_str x -> pf f "(template-str %S)" x
      | Template_end x -> pf f "(template-end %S)" x
      | Eof -> pf f "(eof)"

  let pp f t = Fmt.pf f "`%a`" pp t
  let eq t1 t2 = Stdlib.( = ) t1 t2
end

let rec read r : Token.t =
  let available = Reader.request r 1 in
  if available = 0 then Eof
  else
    match Reader.next r with
    | ' ' -> read r
    | ('a' | 'b' | 'c' | 'd') as x -> Id (string_of_char x)
    | ('1' | '2' | '3' | '4') as x -> Int (Char.code x - 48)
    | '(' -> Lparen
    | '[' -> Lbracket
    | '{' -> Lbrace
    | ')' -> Rparen
    | ']' -> Rbracket
    | '}' -> Rbrace
    | ',' -> Comma
    | ';' -> Semi
    | op0 when is_op_char op0 ->
      let buf = Buffer.create 4 in
      Buffer.add_char buf op0;
      let rec loop () =
        let available = Reader.request r 1 in
        if available >= 1 then
          let c = Reader.peek r in
          if is_op_char c then (
            Reader.drop r 1;
            Buffer.add_char buf c;
            loop ()
          )
      in
      loop ();
      let op = Buffer.contents buf in
      Op op
    | c -> unexpected (string_of_char c)

type lexer = { reader : Reader.t; mutable token : Token.t }

module Lexer = struct
  let of_string str =
    let reader = Reader.of_string str in
    let token = read reader in
    { reader; token }
end

let peek lex = lex.token
let advance lex = lex.token <- read lex.reader

let consume lex expected =
  if Token.eq lex.token expected then lex.token <- read lex.reader
  else
    fail "consume: expected %a, actual %a" Token.pp expected Token.pp lex.token

let rec parse ?power:(rbp = 0) lex =
  let left = start lex in
  let expr = continue lex ~rbp left in
  expr

and start lex =
  let tok = peek lex in
  match peek lex with
  | Id id ->
    advance lex;
    `id id
  | Int int ->
    advance lex;
    `int int
  | Op op -> parse_prefix lex op
  | Lparen -> parse_block lex tok Rparen (fun x -> `parens x)
  | Lbrace -> parse_block lex tok Rbrace (fun x -> `braces x)
  | Lbracket -> parse_block lex tok Rbracket (fun x -> `brackets x)
  | Eof -> fail "start: unexpected eof"
  | tok -> fail "start: unexpected token: %a" Token.pp tok

and continue lex ~rbp left =
  match peek lex with
  | Eof | Rparen | Rbracket | Rbrace -> left
  | Op op ->
    let power = Power.get op in
    let lbp = abs power in
    if lbp > rbp then
      let power = if power < 0 then lbp - 1 else lbp in
      let left' = parse_infix lex ~power op left in
      continue lex ~rbp left'
    else left
  | Comma ->
    let power = Power.comma in
    let lbp = abs power in
    if lbp > rbp then
      let left' =
        let power = if power < 0 then lbp - 1 else lbp in
        parse_seq_delim lex ~delim:Comma ~power left (fun x -> `comma x)
      in
      continue lex ~rbp left'
    else left
  | Semi ->
    let power = Power.semi in
    let lbp = abs power in
    if lbp > rbp then
      let left' =
        let power = if power < 0 then lbp - 1 else lbp in
        parse_seq_delim lex ~delim:Semi ~power left (fun x -> `semi x)
      in
      continue lex ~rbp left'
    else left
  | _ ->
    let left' = parse_seq lex left in
    continue lex ~rbp left'

and parse_seq lex left =
  let rec loop acc =
    match peek lex with
    (* Stop sequence *)
    | Rparen | Rbrace | Rbracket | Eof | Comma | Semi | Op _ -> acc
    | _ ->
      let expr = start lex in
      loop (expr :: acc)
  in
  let acc0 = [ left ] in
  let expr_list = List.rev (loop acc0) in
  `seq expr_list

and parse_prefix lex op =
  advance lex;
  let expr = parse lex in
  `prefix (op, expr)

and parse_infix lex op ~power left =
  advance lex;
  match peek lex with
  | Eof | Rparen | Rbrace | Rbracket | Comma | Semi -> `postfix (op, left)
  | _ ->
    let right = parse ~power lex in
    `infix (op, left, right)

and parse_seq_delim lex ~delim ~power left mk =
  consume lex delim;
  let rec loop acc =
    match peek lex with
    (* Trailing position handling *)
    | Rparen | Rbrace | Rbracket | Eof -> acc
    | _ ->
      let expr = parse ~power lex in
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
  let expr = parse lex in
  consume lex closing;
  mk expr
