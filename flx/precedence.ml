(** Operator precedence and fixity of the tokens.

    Higher numbers represent higher precedence. For example, [*] has higher
    precedence than [+], which results in the expression [a + b * c] being
    parsed as [a + (b * c)].

    The sign of the number controls associativity. Positive numbers have
    left-to-right associativity, like in [(a + b) + c], while negative numbers
    represent right-to-left associativity, like in [a = (b = c)].

    Precedence value [0] has a special meaning: it denotes terminator tokens.
    When a terminator token is encountered, the current expression stops, giving
    control to the parent parser.

    Juxtaposition of tokens has a very high precedence. In a way, juxtaposition
    is like having an invisible operator between [f b]. Since juxtaposition has
    a high precedence, expressions such as [x = f a + 1] are parsed as
    [x = ((f a) + 1)].

    Tokens are classified into two spacing classes:

    - Separator punctuation ([,] [;] [:] [.] [|]) is spacing-blind: it always
      acts as infix/n-ary punctuation regardless of surrounding whitespace.
    - Every other operator resolves its fixity from spacing, uniformly: spaced
      or attached on both sides is infix, attached on the left only is postfix,
      attached on the right only starts a new juxtaposition item (an attached
      prefix, as in [f ~a]).

    Attachment also selects the precedence tier: an operator attached on both
    sides binds in the tight band, above juxtaposition ("things glued together
    parse together"), so [f a+b c] is [f (a+b) c] while [f a + b] is
    [(f a) + b]. Tight operators preserve their relative order and
    associativity. The [.] operator is higher than the tight band, ensuring that
    [f a.b+1] is [f ((a.b) + 1)].

    Precedence bands:
    [terminators 0 < semi < comma < attr < loose ops < juxt < tight ops < dot]

    SEE: https://ocaml.org/manual/5.3/api/Ocaml_operators.html *)

let stop = 0
let semi = 10
let comma = 20
let attr = 25
let juxt = 200
let dot = 400

(* Attached (tight) operators bind above juxtaposition, below [.], preserving
   their relative precedence and associativity. *)
let tighten precedence =
  if precedence < 0 then precedence - juxt else precedence + juxt

let get (tok : Token.t) =
  match tok with
  (* Terminators *)
  | Eof | Rparen | Rbrace | Rbracket | Template_mid _ | Template_end _ -> stop
  | Semi -> semi
  | Comma -> comma
  | Sym "=" -> -30
  | Sym "|" -> 40
  | Sym ":" -> -50
  | Sym "->" -> -55
  | Sym "::" -> 60
  (* NOTE: Should be lower? *)
  | Sym ":=" -> -60
  | Sym "<-" -> -60
  | Sym ("&" | "&&") -> -70
  | Sym "||" -> -70
  | Sym "**" -> -80
  | Sym "." -> dot
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
  | Comment _
  | Char _
  | Int _
  | Backtick
  | Dollar
  | Lparen
  | Lbrace
  | Lbracket
  | Template_start _ -> juxt

(** How a peeked token combines with the expression to its left. *)
type fixity =
  | Stop  (** Terminator: the current expression ends here. *)
  | Juxt_item
      (** The token starts a new juxtaposition item (atoms, blocks, and
          operators attached to the item on their right, as in [f ~a]). *)
  | Infix of int
      (** Signed precedence: negative means right-associative. Attached
          operators carry tight (above-juxtaposition) precedence. *)
  | Postfix of int
      (** Operator attached to its left operand only, as in [a? b]. The
          precedence is always in the tight band. *)

(* Separator punctuation is exempt from the spacing rules. *)
let is_separator = function
  | Token.Comma | Semi | Sym "." | Sym "|" -> true
  | _ -> false

let fixity ({ token; sp } : Token.sp) =
  match token with
  | Eof | Rparen | Rbrace | Rbracket | Template_mid _ | Template_end _ -> Stop
  | token when is_separator token -> Infix (get token)
  | Sym _ -> (
    match sp with
    | `both -> Infix (get token)
    | `none -> Infix (tighten (get token))
    | `left -> Juxt_item
    | `right -> Postfix (abs (tighten (get token)))
  )
  | _ -> Juxt_item
