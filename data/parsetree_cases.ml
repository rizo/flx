(* ====== payload ====== *)

(* [PStr of structure] *)

let e = (1 [@attr let a = 1])

(* [PSig of signature] *)

let e = (1 [@attr: val a : int])

(* [PTyp of core_type] *)

let e = (1 [@attr: int -> int])

(* [PPat of pattern * expression option] *)

let e = (1 [@attr? C _ :: xs when x])

(* ====== core_type ====== *)

(* [Ptyp_any] *)

let e : _ = 1

(* [Ptyp_var of string] *)

let e : 'a = 1

(* [Ptyp_arrow of arg_label * core_type * core_type] *)

let e : int -> string = fun _ -> "abc"
let e : int -> string -> unit = fun _ _ -> ()
let e : l:int -> string = fun ~l:_ -> "abc"
let e : ?l:int -> unit -> string = fun ?l:_ () -> "abc"
let e : int -> (string -> unit) -> bool = fun _ _ -> true

(* [Ptyp_tuple of (string option * core_type) list] *)

let e : int * bool = (1, true)
let e : l1:int * bool = (~l1:1, true)
let e : l1:int * l2:bool = (~l1:1, ~l2:true)

(* [Ptyp_constr of Longident.t loc * core_type list] *)

let e : int = 1
let e : int option = None
let e : (int, string) result = Ok 1

(* [Ptyp_object of object_field list * closed_flag] *)

let e : < x : int ; y : string > = x
let e : < x : int ; y : string ; .. > = x

(* [Ptyp_class of Longident.t loc * core_type list] *)

let e : #cls1 = x
let e : int #cls2 = x
let e : (int, 'b) #t = x

(* [Ptyp_alias of core_type * string loc] *)

let e : int as 'a = x
let e : [> `A ] as 'a = x

(* [Ptyp_variant of row_field list * closed_flag * label list option] *)

let e : [ `A ] = `A
let e : [ `A | `B ] = `A
let e : [< `A | `B ] = `A
let e : [> `A | `B ] = `A
let e : [< `A | `B | `C > `A `B ] = `A
let e : [ | t1 ] = `A
let e : [ t1 | `A ] = `A
let e : [ `A of int ] = `A 1
let e : [ `A of int & bool option & string ] = `A 1
let e : [ `A of  & int ] = `A

(* [Ptyp_poly of string loc list * core_type] *)

let e : 'a. int = 1
let e : 'a 'b. 'a -> int = x

(* [Ptyp_package of package_type] *)

let e : (module M.S) = x

(* [Ptyp_open of Longident.t loc * core_type] *)

let e : M.(a) = x

(* [Ptyp_extension of extension] *)

let e : [%ext 1] = x;;

(* ====== pattern ====== *)

(* [Ppat_any] *)

match x with
| _ -> 1
;;

(* [Ppat_var of string loc] *)

match x with
| a -> 1
;;

(* [Ppat_alias of pattern * string loc] *)

match x with
| 1 as a -> 1
;;

(* [Ppat_constant of constant] *)

match x with
| 1 -> 1
;;

(* [Ppat_interval of constant * constant] *)

match x with
| 'a' .. 'z' -> 1
;;

(* [Ppat_tuple of (string option * pattern) list * Asttypes.closed_flag] *)

match x with
| a, b -> 1
;;

match x with
| ~l1, b -> 1
;;

match x with
| ~l1, ~l2 -> 1
;;

(* [Ppat_construct of Longident.t loc * (string loc list * pattern) option] *)

match x with
| None -> 1
;;

match x with
| Some a -> 1
;;

(* [Ppat_variant of label * pattern option] *)

match x with
| `A -> 1
;;

match x with
| `B a -> 1
;;

(* [Ppat_record of (Longident.t loc * pattern) list * closed_flag] *)

match x with
| { x; y } -> 1
;;

match x with
| { x = x2; y } -> 1
;;

match x with
| { x; _ } -> 1
;;

(* [Ppat_array of pattern list] *)

match x with
| [||] -> 1
;;

match x with
| [| a; _ |] -> 1
;;

(* [Ppat_or of pattern * pattern] *)

match x with
| a | a -> 1
;;

(* [Ppat_constraint of pattern * core_type] *)

match x with
| (a : int) -> 1
;;

(* [Ppat_type of Longident.t loc] *)

match x with
| #t1 -> 1
;;

(* [Ppat_lazy of pattern] *)

let (lazy a) = lazy 1

(* [Ppat_unpack of string option loc] *)

let (module _) = x;;

match x with
| (module M) -> 1
;;

(* [Ppat_exception of pattern] *)

match x with
| _ -> 1
| exception exn -> 2
;;

(* [Ppat_effect of pattern * pattern (* Pattern [effect P P] *)] *)

(* match x with *)
(* | _ -> 1 *)
(* | effect My, k -> 2;; *)

(* [Ppat_extension of extension] *)

match x with
| [%ext 1] -> x
;;

(* [Ppat_open of Longident.t loc * pattern] *)

match x with
| M.(a) -> x
;;

(* ====== expression ====== *)

(* [Pexp_ident of Longident.t loc] *)

x;;
M.a;;
a.A.B.x;;

(* [Pexp_constant of constant] [Pconst_integer of string * char option] *)

let e = 3
let e = 3l
let e = 3L
let e = 3n

(* [Pexp_constant of constant] [Pconst_char of char] *)

let e = 'c'

(* [Pexp_constant of constant] [Pconst_string of string * Location.t * string option] *)

let e = "abc"
let e = {|abc|}
let e = {delim|abc|delim}

(* [Pexp_constant of constant] [Pconst_float of string * char option] *)

let e = 3.4
let e = 2e5
let e = 1.4e-4;;

(* [Pexp_let of rec_flag * value_binding list * expression] *)

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

(* [Pexp_function of function_param list * type_constraint option * function_body] *)

fun x -> 1;;
fun x : int -> 1;;
fun ~l -> 1;;
fun ~l:x2 -> 1;;
fun ?o -> 1;;
fun ?(o = 1) -> 1;;
fun ?o:(o2 = 1) -> 1;;
fun ~x ?b:(b2 : int = 2) c : int -> x;;
fun (type t) -> 1;;
fun (type t) a -> 1;;
fun (type t u) -> 1;;

function
| 0 -> 1
| _ -> 2
;;

fun x -> function
 | 0 -> 1
 | _ -> 2
;;

(* [Pexp_apply of expression * (arg_label * expression) list] *)

f x;;
f x y;;
f ~x ?b;;
f ~x:x2 ?b;;
f ~x:(2 + 2) ?b:None;;

(* [Pexp_match of expression * case list] *)

match x with
| 1 -> 0
| _ -> 1
;;

(* [Pexp_try of expression * case list] *)

try x with Not_found -> 0;;

try x with
| Not_found -> 1
| Failure msg -> 0
;;

(* [Pexp_tuple of (string option * expression) list] *)

1, true;;
~l1:1, true, 'x';;
~l1:1, ~l2:true;;

(* [Pexp_construct of Longident.t loc * expression option] *)

None;;
Some 2;;
More (2, 'x', true);;

(* [Pexp_variant of label * expression option] *)

`A;;
`B 23;;

(* [Pexp_record of (Longident.t loc * expression) list * expression option] *)

{ x = 1 };;
{ r with x = 1 };;
{ (f 1) with x = 1 };;

(* [Pexp_field of expression * Longident.t loc] *)

r.a;;
r.X.a;;

(* [Pexp_setfield of expression * Longident.t loc * expression] *)

r.a <- x;;
r.X.a <- x;;

(* [Pexp_array of expression list] *)

[||];;
[| 1; 2; 3 |];;

(* [Pexp_ifthenelse of expression * expression * expression option] *)

if a then 1 else 2;;
if a then if b then 1 else 2 else 3;;

(* [Pexp_sequence of expression * expression] *)

a;
b
;;

a;
b;
c
;;

(* [Pexp_while of expression * expression] *)

while true do
  1
done
;;

(* [Pexp_for of pattern * expression * expression * direction_flag * expression] *)

for x = 0 to 9 do
  1
done
;;

(* [Pexp_constraint of expression * core_type] *)

(1 : int);;

(* [Pexp_coerce of expression * core_type option * core_type] *)

(x :> t2);;
(x : t1 :> t2);;

(* [Pexp_send of expression * label loc] *)

obj#meth1;;

(* [Pexp_new of Longident.t loc] *)

new a;;
new M.a;;

(* [Pexp_setinstvar of label loc * expression] *)

x <- 1;;

(* [Pexp_override of (label loc * expression) list] *)

{<x = 1; y = 2>};;

(* [Pexp_letmodule of string option loc * module_expr * expression] *)

let module M = struct end in
1
;;

(* [Pexp_letexception of extension_constructor * expression] *)

let exception E in
1
;;

(* [Pexp_assert of expression] *)

assert true;;

(* [Pexp_lazy of expression] *)

lazy 1;;

(* [Pexp_poly of expression * core_type option] *)

object
  method x = 1
end
;;

object
  method x : int = 1
end
;;

(* [Pexp_object of class_structure] *)

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

(* [Pexp_newtype of string loc * expression] *)

fun (type x) -> 1;;

(* [Pexp_pack of module_expr * package_type option] *)

(module X : x);;
(module X : S with type t1 = int);;

(* [Pexp_open of open_declaration * expression] *)

let open M in
1
;;

(* [Pexp_letop of letop] *)

let* x in
1
;;

let* x and+ y = 2 in
1
;;

(* [Pexp_extension of extension] *)

let e = [%ext1];;

(* [Pexp_unreachable] *)

match x with
| _ -> .
;;

(* [Ptype_abstract] *)

type ptype_abstract_1
type ptype_abstract_2 = int
type 'a ptype_abstract_3 = 'a option

(* [Ptype_variant of constructor_declaration list] *)

type ptype_variant_1 = A
type ptype_variant_2 = B | C of int | D of int * bool

(* [Ptype_record of label_declaration list] *)

type ptype_record_1 = { a : int }
type ptype_record_2 = { b : int; mutable c : string }

(* [Ptype_open] *)

type ptype_open_1 = ..

(* [Pcstr_tuple of core_type list] *)

type pcstr_tuple_1 = A2 of int
type pcstr_tuple_2 = B2 of int * bool
type pcstr_tuple_3 = A2 : pcstr_tuple_3
type pcstr_tuple_4 = A2 : int -> pcstr_tuple_4
type 'a pcstr_tuple_5 = B2 : int * bool -> int pcstr_tuple_5

(* [Pcstr_record of label_declaration list] *)

type pcstr_record_1 = A3 of { a : int }
type pcstr_record_2 = B3 of { a : int; mutable b : string }
type pcstr_record_3 = A3 : { a : int } -> pcstr_record_3

(* [Pext_decl of string loc list * constructor_arguments * core_type option] *)

type ptype_open_1 += Pext_decl_1
type ptype_open_1 += Pext_decl_2 of int
type ptype_open_1 += Pext_decl_3 of { a : int }
type ptype_open_1 += Pext_decl_4 : int -> ptype_open_1
type ptype_open_1 += Pext_decl_4 : 'a. 'a * int -> ptype_open_1

(* [Pext_rebind of Longident.t loc] *)

type ptype_open_1 += Pext_rebind_1 = Pext_decl_1

exception Pext_rebind_2 = Not_found

(* [Pcty_constr of Longident.t loc * core_type list] *)

class type pcty_constr_1 = ct1
class type pcty_constr_2 = [int] ct2
class type pcty_constr_3 = [int, string] M.ct3

(* [Pcty_signature of class_signature] *)

class type pcty_signature_1 = object end

class type pcty_signature_2 = object ('self)
  method m : int
end

(* [Pcty_arrow of arg_label * core_type * class_type] *)

class pcty_arrow_1 : int -> object end = fun _ -> object end
class pcty_arrow_2 : l:int -> object end = fun ~l:_ -> object end
class pcty_arrow_3 : ?l:int -> object end = fun ?l:_ -> object end

(* [Pcty_extension of extension] *)

class type pcty_extension_1 = [%ext]

(* [Pcty_open of open_description * class_type] *)

class type pcty_open_1 =
  let open M in
object
  end

(* [Pctf_inherit of class_type] *)

class type pctf_inherit_1 = object
  inherit ct1
end

(* [Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)] *)

class type pctf_val_1 = object
  val v1 : int
  val mutable v2 : int
  val virtual v3 : int
  val mutable virtual v4 : int
end

(* [Pctf_method of (label loc * private_flag * virtual_flag * core_type)] *)

class type pctf_method_1 = object
  method m1 : int
  method private m2 : int
  method virtual m3 : int
  method private virtual m4 : int
end

(* [Pctf_constraint of (core_type * core_type)] *)

class type ['a] pctf_constraint_1 = object
  constraint 'a = int
end

(* [Pctf_attribute of attribute] *)

class type pctf_attribute_1 = object
  [@@@attr]
end

(* [Pctf_extension of extension] *)

class type pctf_extension_1 = object
  [%%ext]
end

(* [Pcl_constr of Longident.t loc * core_type list] *)

class pcl_constr_1 = cls1
class pcl_constr_2 = [int] cls2
class pcl_constr_3 = [int, string] M.cls3

(* [Pcl_structure of class_structure] *)

class pcl_structure_1 = object end

class pcl_structure_2 =
  object (self)
    method m = 1
  end

(* [Pcl_fun of arg_label * expression option * pattern * class_expr] *)

class pcl_fun_1 = fun x -> object end
class pcl_fun_2 = fun ~l -> object end
class pcl_fun_3 = fun ?(o = 1) x -> object end

(* [Pcl_apply of class_expr * (arg_label * expression) list] *)

class pcl_apply_1 = cls1 1
class pcl_apply_2 = cls1 ~l:1 ?o:None

(* [Pcl_let of rec_flag * value_binding list * class_expr] *)

class pcl_let_1 =
  let e = 1 in
  object end

class pcl_let_2 =
  let rec x = 1 and y = 2 in
  object end

(* [Pcl_constraint of class_expr * class_type] *)

class pcl_constraint_1 = (object end : object end)

(* [Pcl_extension of extension] *)

class pcl_extension_1 = [%ext]

(* [Pcl_open of open_description * class_expr] *)

class pcl_open_1 =
  let open M in
  object end

(* [Pcf_inherit of override_flag * class_expr * string loc option] *)

class pcf_inherit_1 =
  object
    inherit cls1
  end

class pcf_inherit_2 =
  object
    inherit cls1 as super
  end

class pcf_inherit_3 =
  object
    inherit! cls1
  end

(* [Pcf_val of (label loc * mutable_flag * class_field_kind)] *)

class virtual pcf_val_1 =
  object
    val v1 = 1
    val mutable v2 = 2
    val! v3 = 3
    val virtual v4 : int
    val mutable virtual v5 : int
  end

(* [Pcf_method of (label loc * private_flag * class_field_kind)] *)

class virtual pcf_method_1 =
  object
    method m1 = 1
    method private m2 = 2
    method! m3 = 3
    method virtual m4 : int
    method private virtual m5 : int
  end

(* [Pcf_constraint of (core_type * core_type)] *)

class ['a] pcf_constraint_1 =
  object
    constraint 'a = int
  end

(* [Pcf_initializer of expression] *)

class pcf_initializer_1 =
  object
    initializer print_endline "init"
  end

(* [Pcf_attribute of attribute] *)

class pcf_attribute_1 =
  object
    [@@@attr]
  end

(* [Pcf_extension of extension] *)

class pcf_extension_1 =
  object
    [%%ext]
  end

(* [Pmty_ident of Longident.t loc] *)

module type Pmty_ident_1 = S
module type Pmty_ident_2 = M.S

(* [Pmty_signature of signature] *)

module type Pmty_signature_1 = sig end

module type Pmty_signature_2 = sig
  val x : int
end

(* [Pmty_functor of functor_parameter * module_type] *)

module type Pmty_functor_1 = functor (X : S) -> sig end
module type Pmty_functor_2 = functor (_ : S) -> sig end
module type Pmty_functor_3 = functor () -> sig end
module type Pmty_functor_4 = S -> sig end

(* [Pmty_with of module_type * with_constraint list] *)

module type Pmty_with_1 = S with type t = int
module type Pmty_with_2 = S with type t = int and module M1 = M2

(* [Pmty_typeof of module_expr] *)

module type Pmty_typeof_1 = module type of M
module type Pmty_typeof_2 = module type of struct end

(* [Pmty_extension of extension] *)

module type Pmty_extension_1 = [%ext]

(* [Pmty_alias of Longident.t loc] *)

module type Pmty_alias_1 = sig
  module X = M
end

(* [Psig_value of value_description] *)

module type Psig_value_1 = sig
  val x : int
  external f : int -> int = "f_stub"
end

(* [Psig_type of rec_flag * type_declaration list] *)

module type Psig_type_1 = sig
  type t
  type nonrec u = int

  type v = A of int
  and w = { a : v }
end

(* [Psig_typesubst of type_declaration list] *)

module type Psig_typesubst_1 = sig
  type t := int
end

(* [Psig_typext of type_extension] *)

module type Psig_typext_1 = sig
  type t = ..
  type t += A of int
end

(* [Psig_exception of type_exception] *)

module type Psig_exception_1 = sig
  exception E of int
end

(* [Psig_module of module_declaration] *)

module type Psig_module_1 = sig
  module X : sig end
  module Y = M
end

(* [Psig_modsubst of module_substitution] *)

module type Psig_modsubst_1 = sig
  module X := M
end

(* [Psig_recmodule of module_declaration list] *)

module type Psig_recmodule_1 = sig
  module rec X : sig end
  and Y : sig end
end

(* [Psig_modtype of module_type_declaration] *)

module type Psig_modtype_1 = sig
  module type T
  module type U = sig end
end

(* [Psig_modtypesubst of module_type_declaration] *)

module type Psig_modtypesubst_1 = sig
  module type T := sig end
end

(* [Psig_open of open_description] *)

module type Psig_open_1 = sig
  open M
  open! M.N
end

(* [Psig_include of include_description] *)

module type Psig_include_1 = sig
  include S
  include module type of M
  include module type of M with type t = int
  include module type of M with type t := int
  include module type of M with type t = int and type u = string
end

(* [Psig_class of class_description list] *)

module type Psig_class_1 = sig
  class c1 : object end
  class c2 : int -> object end
  class ['a] c3 : object end
  class virtual c4 : object end
end

(* [Psig_class_type of class_type_declaration list] *)

module type Psig_class_type_1 = sig
  class type ct1 = object end
  class type ['a] ct2 = object end
end

(* [Psig_attribute of attribute] *)

module type Psig_attribute_1 = sig
  [@@@attr]
end

(* [Psig_extension of extension * attributes] *)

module type Psig_extension_1 = sig
  [%%ext]
  [%%ext2] [@@attr]
end

(* [Pwith_type of Longident.t loc * type_declaration] *)

module type Pwith_type_1 = S with type t = int
module type Pwith_type_2 = S with type 'a t = 'a list
module type Pwith_type_3 = S with type t = private int

(* [Pwith_module of Longident.t loc * Longident.t loc] *)

module type Pwith_module_1 = S with module M1 = M2

(* [Pwith_modtype of Longident.t loc * module_type] *)

module type Pwith_modtype_1 = S with module type T = sig end

(* [Pwith_modtypesubst of Longident.t loc * module_type] *)

module type Pwith_modtypesubst_1 = S with module type T := sig end

(* [Pwith_typesubst of Longident.t loc * type_declaration] *)

module type Pwith_typesubst_1 = S with type t := int

(* [Pwith_modsubst of Longident.t loc * Longident.t loc] *)

module type Pwith_modsubst_1 = S with module M1 := M2

(* [Pmod_ident of Longident.t loc] *)

module Pmod_ident_1 = X
module Pmod_ident_2 = X.Y

(* [Pmod_structure of structure] *)

module Pmod_structure_1 = struct end

module Pmod_structure_2 = struct
  let e = 1
end

(* [Pmod_functor of functor_parameter * module_expr] *)

module Pmod_functor_1 (X : S) = struct end
module Pmod_functor_2 (_ : S) = struct end
module Pmod_functor_3 () = struct end
module Pmod_functor_4 = functor (X : S) -> struct end

(* [Pmod_apply of module_expr * module_expr (** [ME1(ME2)] *)] *)

module Pmod_apply_1 = F (X)
module Pmod_apply_2 = F (X) (Y)

(* [Pmod_apply_unit of module_expr (** [ME1()] *)] *)

module Pmod_apply_unit_1 = F ()

(* [Pmod_constraint of module_expr * module_type] *)

module Pmod_constraint_1 : S = X
module Pmod_constraint_2 : S = struct end

(* [Pmod_unpack of expression] *)

module Pmod_unpack_1 = (val x)
module Pmod_unpack_2 = (val x : S)

(* [Pmod_extension of extension] *)

module Pmod_extension_1 = [%ext];;

(* [Pstr_eval of expression * attributes] *)

1 + 1;;

(* [Pstr_value of rec_flag * value_binding list] *)

let e = 1
let rec x () = x ()

let e1 = 1
and e2 = 2

(* [Pstr_primitive of value_description] *)

external x : int -> int = "prim_stub"
external x : int -> int = "prim_stub" "prim_stub_native"

(* [Pstr_type of rec_flag * type_declaration list] *)

type t = int
type nonrec t = int

type t1 = A4 of int
and t2 = B4 of bool

(* [Pstr_typext of type_extension] *)

type t += Pstr_typext_1 of bool

(* [Pstr_exception of type_exception] *)

exception Pstr_exception_1
exception Pstr_exception_2 of int * string

(* [Pstr_module of module_binding] *)

module Pstr_module_1 = struct end

(* [Pstr_recmodule of module_binding list] *)

module rec M1 : S = struct end
and M2 : S = struct end

(* [Pstr_modtype of module_type_declaration] *)

module type Mt = sig end

(* [Pstr_open of open_declaration] *)

open M
open! M.N

open struct
  let e = 1
end

(* [Pstr_class of class_declaration list] *)

class c = object end
class virtual ['a] c = object end

class c1 = object end

and c2 = object end

(* [Pstr_class_type of class_type_declaration list] *)

class type ct = object end

class type ct1 = object end

and ct2 = object end

(* [Pstr_include of include_declaration] *)

include M

include struct
  let e = 1
end

(* [Pstr_attribute of attribute] *)

[@@@attr]
[@@@attr "payload"]

(* [Pstr_extension of extension * attributes] *)

[%%ext]
[%%ext 1] [@@attr]

(* [Pvc_constraint of {] *)

let e : int = 1
let e : 'a. 'a -> 'a = fun x -> x
let e : type a. a -> a = fun x -> x

(* [Pvc_coercion of {ground:core_type option; coercion:core_type }] *)

let e :> int = x
let e : t1 :> t2 = x;;

(* ====== toplevel ====== *)
#show

#use "file.ml"
