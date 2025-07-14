type t =
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

let pp =
  let pf = Format.fprintf in
  fun f token ->
    match token with
    | Id x -> pf f "%s" x
    | Lparen -> pf f "'('"
    | Rparen -> pf f "')'"
    | Lbrace -> pf f "'{'"
    | Rbrace -> pf f "'}'"
    | Lbracket -> pf f "'['"
    | Rbracket -> pf f "']'"
    | Backtick -> pf f "'`'"
    | Comma -> pf f "','"
    | Semi -> pf f "';'"
    | Str x -> pf f "(str %S)" x
    | Char x -> pf f "(char %c)" x
    | Int x -> pf f "(int %d)" x
    | Sym x -> pf f "(sym '%s')" x
    | Template_start x -> pf f "(template-start %S)" x
    | Template_mid x -> pf f "(template-mid %S)" x
    | Template_end x -> pf f "(template-end %S)" x
    | Eof -> pf f "(eof)"

let eq t1 t2 = Stdlib.( = ) t1 t2
