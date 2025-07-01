type t =
  [ `id of string
  | `op of string
  | `int of int
  | `str of string
  | `char of char
  | `parens of t
  | `brackets of t
  | `braces of t
  | `prefix of string * t
  | `infix of string * t * t
  | `postfix of string * t
  | `comma of t list
  | `semi of t list
  | `dot of t list
  | `pipe of t list
  | `seq of t list
  | `quote of t
  | `template of t list ]

let rec pp f (t : t) =
  match t with
  | `id x -> Fmt.pf f "%s" x
  | `op x -> Fmt.pf f "%s" x
  | `int x -> Fmt.pf f "%d" x
  | `str x -> Fmt.pf f "%S" x
  | `char x -> Fmt.pf f "%C" x
  | `parens x -> Fmt.pf f "@[<hv2>((_)@ %a@])" pp x
  | `brackets x -> Fmt.pf f "@[<hv2>([_]@ %a@])" pp x
  | `braces x -> Fmt.pf f "@[<hv2>({_}@ %a@])" pp x
  | `prefix (fix, x) -> Fmt.pf f "@[<hv2>(%s_@ %a)@]" fix pp x
  | `infix (fix, x, y) -> Fmt.pf f "@[<hv2>(_%s_@ %a@ %a)@]" fix pp x pp y
  | `postfix (fix, x) -> Fmt.pf f "@[<hv2>(_%s@ %a)@]" fix pp x
  | `comma [] -> Fmt.pf f "(,)"
  | `comma xs -> Fmt.pf f "(, @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `dot xs -> Fmt.pf f "(. @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `pipe xs -> Fmt.pf f "(| @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `semi [] -> Fmt.pf f "(;)"
  | `semi xs -> Fmt.pf f "(; @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `seq [] -> Fmt.pf f "()"
  | `quote x -> Fmt.pf f "`(%a)" pp x
  | `seq xs -> Fmt.pf f "(_ @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `template [] -> Fmt.pf f "($)"
  | `template xs -> Fmt.pf f "($ @[<hv2>%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
