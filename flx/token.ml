open struct
  let pf = Format.fprintf
end

type t =
  | Id of string
  | Sym of string
  | Int of int
  | Str of string
  | Char of char
  | Comment of string
  | Backtick
  | Dollar
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

(* TODO: Update t to allow tight only on sym. Others are not important. *)
type sp = { token : t; tight : [ `left | `right | `both | `none ] }

let pp f token =
  match token with
  | Id x -> pf f "%s" x
  | Lparen -> pf f "'('"
  | Rparen -> pf f "')'"
  | Lbrace -> pf f "'{'"
  | Rbrace -> pf f "'}'"
  | Lbracket -> pf f "'['"
  | Rbracket -> pf f "']'"
  | Backtick -> pf f "'`'"
  | Dollar -> pf f "'$'"
  | Comma -> pf f "','"
  | Semi -> pf f "';'"
  | Str x -> pf f "(str %S)" x
  | Char x -> pf f "(char %c)" x
  | Comment x -> pf f "(comment %S)" x
  | Int x -> pf f "(int %d)" x
  | Sym x -> pf f "(sym '%s')" x
  | Template_start x -> pf f "(template-start %S)" x
  | Template_mid x -> pf f "(template-mid %S)" x
  | Template_end x -> pf f "(template-end %S)" x
  | Eof -> pf f "(eof)"

let pp_sp f x =
  match x.tight with
  | `left -> pf f "[_%a]" pp x.token
  | `right -> pf f "[%a_]" pp x.token
  | `none -> pf f "[%a]" pp x.token
  | `both -> pf f "[_%a_]" pp x.token

let eq t1 t2 = Stdlib.( = ) t1 t2
