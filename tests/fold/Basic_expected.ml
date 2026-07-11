let e : _ = 1
let e : 'a = 1
let e_arrow_1 : int -> string = fun _ -> "abc"
let e_arrow_2 : int -> string -> unit = fun _ _ -> ()
let e_arrow_3 : l:int -> string = fun ~l:_ -> "abc"
let e_arrow_4 : ?l:int -> unit -> string = fun ?l:_ () -> "abc"
let e_arrow_5 : int -> (string -> unit) -> bool = fun _ _ -> true
let ptyp_tuple_1 : int * bool = (1, true)
let ptyp_tuple_2 : l1:int * bool = (~l1:1, true)
let ptyp_tuple_3 : l1:int * l2:bool = (~l1:1, ~l2:true)
let e : int = 1
let e : int option = None
let e : (int, string) result = Ok 1
let ptyp_alias_1 : int as 'a = x
let ptyp_alias_2 : [> `A ] as 'a = x
let e : [ `A ] = `A
let e : [ `A | `B ] = `A
let e : [> `A | `B ] = `A
let e : [ `A of int ] = `A 1
let e : 'a. int = 1
let e : 'a 'b. 'a -> int = x;;

match x with
| _ -> 1
;;

match x with
| a -> 1
;;

match x with
| 1 as a -> 1
;;

match x with
| 1 -> 1
;;

match x with
| 'a' .. 'z' -> 1
;;

match x with
| a, b -> 1
;;

match x with
| ~l1, b -> 1
;;

match x with
| ~l1, ~l2 -> 1
;;

match x with
| None -> 1
;;

match x with
| Some a -> 1
;;

match x with
| `A -> 1
;;

match x with
| `B a -> 1
;;

match x with
| { x; y } -> 1
;;

match x with
| { x = x2; y } -> 1
;;

match x with
| { x; _ } -> 1
;;

match x with
| [||] -> 1
;;

match x with
| [| a; _ |] -> 1
;;

match x with
| a | a -> 1
;;

match x with
| (a : int) -> 1
;;

match x with
| #t1 -> 1
;;

let (lazy a) = lazy 1
let (module _) = x;;

match x with
| (module M) -> 1
;;

match x with
| _ -> 1
| exception err -> 2
;;

match x with
| M.(a) -> x
;;

x;;
M.a;;
a.A.B.x;;

let e = 3
let e = 3l
let e = 3L
let e = 3n
let e = 'c'
let e = "abc";;

let e = 1 in
x
;;

let rec x = 1 in
x
;;

let e = 1 and y = 2 in
x + y
;;

let rec x = 1 and y = 2 in
x + y
;;

fun x -> 1;;
fun ~l -> 1;;
fun ~l:x2 -> 1;;
fun ?o -> 1;;
fun ?(o = 1) -> 1;;
fun ?o:(o2 = 1) -> 1;;
fun (type t) -> 1;;
fun (type t) a -> 1;;
fun (type t) (type u) -> 1;;

function
| 0 -> 1
| _ -> 2
;;

fun x ->
  (function
  | 0 -> 1
  | _ -> 2
  )
;;

f x;;
f x y;;
f ~x ?b;;
f ~x:x2 ?b;;
f ~x:(2 + 2) ?b:None;;

match x with
| 1 -> 0
| _ -> 1
;;

try x with Not_found -> 0;;

try x with
| Not_found -> 1
| Failure msg -> 0
;;

1, true;;
~l1:1, true, 'x';;
~l1:1, ~l2:true;;
None;;
Some 2;;
More (2, 'x', true);;
`A;;
`B 23;;
{ x = 1 };;
{ r with x = 1 };;
{ (f 1) with x = 1 };;
r.a;;
r.X.a;;
r.a <- x;;
r.X.a <- x;;
[||];;
[| 1; 2; 3 |];;
if a then 1 else 2;;
if a then if b then 1 else 2 else 3;;
[| a; b |];;
[| a; b; c |];;

while true do
  1
done
;;

for x = 0 to 9 do
  [| 1 |]
done
;;

(1 : int);;
(x :> t2);;
(x : t1 :> t2);;
obj#meth1;;
new a;;
new M.a;;
x <- 1;;
{<x = 1; y = 2>};;
assert true;;
lazy 1;;

object
  method x = 1
end
;;

object
  method x : int = 1
end
;;

object end;;

object
  method x = 1
  method y = 2
end
;;

object (myself)
  method x = 1
end
;;

object (a as myself)
  method x = 1
end
;;

fun (type x) -> 1;;

let e = [%ext1];;

match x with
| _ -> .
;;

type nonrec ptype_abstract_1
type ptype_abstract_2 = int
type 'a ptype_abstract_3 = 'a option
type ptype_variant_1 = A
type ptype_variant_2 = B | C of int | D of int * bool
type ptype_record_1 = { a : int }
type ptype_record_2 = { b : int; mutable c : string }
type ptype_open_1 = ..
type pcstr_tuple_1 = A2 of int
type pcstr_tuple_2 = B2 of int * bool
type pcstr_record_1 = A3 of { a : int }
type pcstr_record_2 = B3 of { a : int; mutable b : string }
type ptype_open_1 += Pext_decl_1
type ptype_open_1 += Pext_decl_2 of int
type ptype_open_1 += Pext_decl_3 of { a : int }
type ptype_open_1 += Pext_rebind_1 = Pext_decl_1

exception Pext_rebind_2 = Not_found

module type Pmty_ident_1 = S
module type Pmty_ident_2 = M.S
module type Pmty_signature_1 = sig end

module type Pmty_signature_2 = sig
  val x : int
end

module type Pmty_typeof_1 = module type of M
module type Pmty_typeof_2 = module type of struct end
module type Pmty_extension_1 = [%ext]

module type Pmty_alias_1 = sig
  module X = M
end

module type Psig_value_1 = sig
  val x : int
  external f : int -> int = "f_stub"
end

module type Psig_type_1 = sig
  type nonrec t
  type nonrec u = int

  type v = A of int
  and w = { a : v }
end

module type Psig_typext_1 = sig
  type t = ..
  type t += A of int
end

module type Psig_exception_1 = sig
  exception E of int
end

module type Psig_module_1 = sig
  module X : sig end
  module Y = M
end

module type Psig_modtype_1 = sig
  module type T
  module type U = sig end
end

module type Psig_open_1 = sig
  open M
  open! M.N
end

module type Psig_include_1 = sig
  include S
  include module type of M
end

module type Psig_attribute_1 = sig
  [@@@attr]
end

module type Psig_extension_1 = sig
  [%%ext]
end

module Pmod_ident_1 = X
module Pmod_ident_2 = X.Y
module Pmod_structure_1 = struct end

module Pmod_structure_2 = struct
  let e = 1
end

module Pmod_apply_1 = F (X)
module Pmod_apply_2 = F (X) (Y)
module Pmod_apply_unit_1 = F ()
module Pmod_constraint_1 : S = X
module Pmod_constraint_2 : S = struct end
module Pmod_unpack_1 = (val x)
module Pmod_extension_1 = [%ext];;

1 + 1;;

let e = 1
let rec x () = x ()

let e1 = 1
and e2 = 2

external x : int -> int = "prim_stub"
external x : int -> int = "prim_stub" "prim_stub_native"

type t = int
type nonrec t = int

type t1 = A4 of int
and t2 = B4 of bool

type t += Pstr_typext_1 of bool

exception Pstr_exception_1
exception Pstr_exception_2 of int * string

module Pstr_module_1 = struct end

module type Mt = sig end

open M
open! M.N

open struct
  let e = 1
end

include M

include struct
  let e = 1
end

[@@@attr]
[@@@attr "payload"]

[%%ext]

let e : int = 1
let e : 'a. 'a -> 'a = fun x -> x
let e : type a. a -> a = fun x -> x
let e :> int = x
let e : t1 :> t2 = x
