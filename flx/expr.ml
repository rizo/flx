open Prelude

type tier = [ `tight | `loose ]

type t =
  [ `id of string
  | `op of string
  | `int of int
  | `str of string
  | `char of char
  | `comment of string
  | `parens of t
  | `brackets of t
  | `braces of t
  | `prefix of tier * string * t
  | `infix of tier * string * t * t
  | `postfix of tier * string * t
  | `comma of t list
  | `semi of t list
  | `dot of t list
  | `pipe of t list
  | `seq of t list
  | `quote of t
  | `unquote of t
  | `attr of t * t option
  | `template of t list ]

let rec pp f (t : t) =
  match t with
  | `id x -> Fmt.pf f "%s" x
  | `op x -> Fmt.pf f "%s" x
  | `int x -> Fmt.pf f "%d" x
  | `str x -> Fmt.pf f "%S" x
  | `char x -> Fmt.pf f "%C" x
  | `comment x -> Fmt.pf f "(// %S)" x
  | `parens x -> Fmt.pf f "@[<hv2>((_)@ %a@])" pp x
  | `brackets x -> Fmt.pf f "@[<hv2>([_]@ %a@])" pp x
  | `braces x -> Fmt.pf f "@[<hv2>({_}@ %a@])" pp x
  | `prefix (`tight, fix, x) -> Fmt.pf f "@[<hv2>(%s.@ %a)@]" fix pp x
  | `prefix (`loose, fix, x) -> Fmt.pf f "@[<hv2>(%s_@ %a)@]" fix pp x
  | `infix (`tight, fix, x, y) ->
    Fmt.pf f "@[<hv2>(.%s.@ %a@ %a)@]" fix pp x pp y
  | `infix (`loose, fix, x, y) ->
    Fmt.pf f "@[<hv2>(_%s_@ %a@ %a)@]" fix pp x pp y
  | `postfix (`tight, fix, x) -> Fmt.pf f "@[<hv2>(.%s@ %a)@]" fix pp x
  | `postfix (`loose, fix, x) -> Fmt.pf f "@[<hv2>(_%s@ %a)@]" fix pp x
  | `dot xs -> Fmt.pf f "(. @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `pipe xs -> Fmt.pf f "(| @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `semi [] -> Fmt.pf f "(;)"
  | `semi xs -> Fmt.pf f "@[<hv2>(;@ %a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `comma [] -> Fmt.pf f "(,)"
  | `comma xs -> Fmt.pf f "@[<hv2>(,@ %a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `seq [] -> Fmt.pf f "()"
  | `quote x -> Fmt.pf f "(`%a)" pp x
  | `unquote x -> Fmt.pf f "($ %a)" pp x
  | `attr (a, None) -> Fmt.pf f "@[(@@%a@])" pp a
  | `attr (a, Some x) -> Fmt.pf f "@[<hv2>(@@%a@ %a@])" pp a pp x
  | `seq xs -> Fmt.pf f "(_ @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
  | `template [] -> Fmt.pf f "(\"\")"
  | `template xs -> Fmt.pf f "(@[<hv2>%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
