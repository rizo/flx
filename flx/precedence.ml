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

let juxt = 200

let get (tok : Token.t) =
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
  (* NOTE: ! is prefix-only *)
  (* | Sym "!" -> 60 *)
  (* NOTE: Should be lower? *)
  | Sym ":=" -> -60
  | Sym "<-" -> -60
  | Sym ("&" | "&&") -> -70
  | Sym "||" -> -70
  | Sym "**" -> -80
  | Sym "." -> 300
  | Sym "!" -> juxt
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
