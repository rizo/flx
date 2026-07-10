(* --- constant --- *)

(** [Pconst_integer of string * char option] *)

let pconst_integer_1 = 3
let pconst_integer_2 = 3l
let pconst_integer_3 = 3L
let pconst_integer_4 = 3n

(** [Pconst_char of char] *)

let pconst_char_1 = 'c'

(** [Pconst_string of string * Location.t * string option] *)

let pconst_string_1 = "constant"
let pconst_string_2 = {|other constant|}
let pconst_string_3 = {delim|other constant|delim}

(** [Pconst_float of string * char option] *)

let pconst_float_1 = 3.4
let pconst_float_2 = 2e5
let pconst_float_3 = 1.4e-4

(* --- payload --- *)

(** [PStr of structure] *)

let pstr_1 = (1 [@attr let a = 1])

(** [PSig of signature] *)

let psig_1 = (1 [@attr: val a : int])

(** [PTyp of core_type] *)

let ptyp_1 = (1 [@attr: int -> int])

(** [PPat of pattern * expression option] *)

let ppat_1 = (1 [@attr? C _ :: xs when x])

(* -- core_type -- *)

(** [Ptyp_any] *)

let ptyp_any : _ = 1

(** [Ptyp_var of string] *)

let ptyp_var : 'a = 1

(** [Ptyp_arrow of arg_label * core_type * core_type] *)

let ptyp_arrow_1 : int -> string = any ()
let ptyp_arrow_2 : int -> string -> unit = fun _ _ -> ()
let ptyp_arrow_3 : l:int -> string = fun ~l:_ -> "abc"
let ptyp_arrow_4 : ?l:int -> unit -> string = fun ?l:_ () -> "abc"
let ptyp_arrow_5 : int -> (string -> unit) -> bool = fun _ _ -> true

(** [Ptyp_tuple of (string option * core_type) list] *)

let ptyp_tuple_1 : int * bool = (1, true)
let ptyp_tuple_2 : l1:int * bool = (~l1:1, true)
let ptyp_tuple_3 : l1:int * l2:bool = (~l1:1, ~l2:true)

(** [Ptyp_constr of Longident.t loc * core_type list] *)

let ptyp_constr_1 : int = 1
let ptyp_constr_2 : int option = None
let ptyp_constr_3 : (int, string) result = Ok 1

(** [Ptyp_object of object_field list * closed_flag] *)

let ptyp_object_1 : < x : int ; y : string > =
  object
    method x = 1
    method y = "abc"
  end
;;

let ptyp_object_2 : < x : int ; y : string ; .. > =
  object
    method x = 1
    method y = "abc"
  end
;;

(** [Ptyp_class of Longident.t loc * core_type list] *)

let ptyp_class_1 : #cls1 = x
let ptyp_class_2 : int #cls2 = x
let ptyp_class_3 : (int, 'b) #t = x

(** [Ptyp_alias of core_type * string loc  (** [T as 'a]. *)] *)

let pty_alias_1 : int as 'a = x
let pty_alias_1 : [> `A ] as 'a = x

(** [Ptyp_variant of row_field list * closed_flag * label list option] *)

let ptyp_variant_1 : [ `A ] = `A
let ptyp_variant_2 : [ `A | `B ] = `A
let ptyp_variant_3 : [< `A | `B ] = `A
let ptyp_variant_4 : [> `A | `B ] = `A
let ptyp_variant_5 : [< `A | `B | `C > `A `B ] = `A
let ptyp_variant_6 : [ | t1 ] = `A
let ptyp_variant_7 : [ t1 | `A ] = `A
let ptyp_variant_8 : [ `A of int ] = `A 1
let ptyp_variant_8 : [ `A of int & bool option & string ] = `A 1
let ptyp_variant_8 : [ `A of  & int ] = `A

(** [Ptyp_poly of string loc list * core_type] *)

let ptyp_poly_1 : 'a. int = x
let ptyp_poly_1 : 'a 'b. 'a -> int = x

(** [Ptyp_package of package_type  (** [(module S)]. *)] *)
let ptyp_package_1 : (module Map.S) = x

(** [Ptyp_open of Longident.t loc * core_type (** [M.(T)] *)] *)

let ptyp_open_1 : M.(a) = x

(** [Ptyp_extension of extension  (** [[%id]]. *)] *)

let ptyp_extension_1 : [%ext 1] = x;;

(** [Ppat_any  (** The pattern [_]. *)] *)

match x with
| _ -> 1
;;

(** [Ppat_var of string loc  (** A variable pattern such as [x] *)] *)

match x with
| a -> 1
;;

(** [Ppat_alias of pattern * string loc] *)

match x with
| 1 as a -> 1
;;

(** [Ppat_constant of constant] *)

match x with
| 1 -> 1
;;

(** [Ppat_interval of constant * constant] *)

match x with
| 'a' .. 'z' -> 1
;;

(** [Ppat_tuple of (string option * pattern) list * Asttypes.closed_flag] *)

match x with
| a, b -> 1
;;

match x with
| ~l1, b -> 1
;;

match x with
| ~l1, ~l2 -> 1
;;

(** [Ppat_construct of Longident.t loc * (string loc list * pattern) option] *)

match x with
| None -> 1
;;

match x with
| Some a -> 1
;;

(** [Ppat_variant of label * pattern option] *)

match x with
| `A -> 1
;;

match x with
| `B a -> 1
;;

(** [Ppat_record of (Longident.t loc * pattern) list * closed_flag] *)

match x with
| { x; y } -> 1
;;

match x with
| { x = x2; y } -> 1
;;

match x with
| { x; _ } -> 1
;;

(** [Ppat_array of pattern list  (** Pattern [[| P1; ...; Pn |]] *)] *)

match x with
| [||] -> 1
;;

match x with
| [| a; _ |] -> 1
;;

(** [Ppat_or of pattern * pattern  (** Pattern [P1 | P2] *)] *)

match x with
| a | a -> 1
;;

(** [Ppat_constraint of pattern * core_type  (** Pattern [(P : T)] *)] *)

match x with
| (a : int) -> 1
;;

(** [Ppat_type of Longident.t loc  (** Pattern [#tconst] *)] *)

match x with
| #t1 -> 1
;;

(** [Ppat_lazy of pattern  (** Pattern [lazy P] *)] *)

let (lazy a) = lazy 1

(** [Ppat_unpack of string option loc] *)

let (module _) = x;;

match x with
| (module M) -> 1
;;

(** [Ppat_exception of pattern  (** Pattern [exception P] *)] *)

match x with
| _ -> 1
| exception exn -> 2
;;

(** [Ppat_effect of pattern * pattern (* Pattern [effect P P] *)] *)

(* match x with *)
(* | _ -> 1 *)
(* | effect My, k -> 2;; *)

(** [Ppat_extension of extension  (** Pattern [[%id]] *)] *)

match x with
| [%ext 1] -> x
;;

(** [Ppat_open of Longident.t loc * pattern  (** Pattern [M.(P)] *)] *)

match x with
| M.(a) -> x
;;

(** [Pexp_ident of Longident.t loc] *)

x;;
M.a;;
a.A.B.x;;

(** [Pexp_constant of constant] *)

42;;

(** [Pexp_let of rec_flag * value_binding list * expression] *)

let x = 1 in
x
;;

let rec x = 1 in
x
;;

let x = 1 and y = 2 in
x + y
;;

let rec x = 1 and y = 2 in
x + y
;;

(** [Pexp_function of function_param list * type_constraint option * function_body] *)

fun x -> x;;
fun x y -> x + y;;
fun x y : int -> x + y;;
fun ~x (int : int) : int -> x;;
fun ~x (int : int) ?b c : int -> x;;
fun ~x ?(b = 2) c : int -> x;;
fun ~x ?b:(b2 = 2) c : int -> x;;
fun ~x ?b:(b2 : int = 2) c : int -> x;;

(** [Pexp_apply of expression * (arg_label * expression) list] *)

f x;;
f x y;;
f ~x ?b;;
f ~x:x2 ?b;;
f ~x:(2 + 2) ?b:None;;

(** [Pexp_match of expression * case list] *)

match x with
| 1 -> 0
| _ -> 1
;;

(** [Pexp_try of expression * case list] *)

try x with Not_found -> 0;;

try x with
| Not_found -> 1
| Failure msg -> 0
;;

(** [Pexp_tuple of (string option * expression) list] *)

1, true;;
~l1:1, true, 'x';;
~l1:1, ~l2:true;;

(** [Pexp_construct of Longident.t loc * expression option] *)

None;;
Some 2;;
More (2, 'x', true);;

(** [Pexp_variant of label * expression option] *)

`A;;
`B 23;;

(** [Pexp_record of (Longident.t loc * expression) list * expression option] *)

{ x = 1 };;
{ r with x = 1 };;
{ (f 1) with x = 1 };;

(** [Pexp_field of expression * Longident.t loc  (** [E.l] *)] *)

r.a;;
r.X.a;;

(** [Pexp_setfield of expression * Longident.t loc * expression] *)

r.a <- x;;
r.X.a <- x;;

(** [Pexp_array of expression list  (** [[| E1; ...; En |]] *)] *)

[||];;
[| 1; 2; 3 |];;

(** [Pexp_ifthenelse of expression * expression * expression option] *)

if a then 1 else 2;;
if a then if b then 1 else 2 else 3;;

(** [Pexp_sequence of expression * expression  (** [E1; E2] *)] *)

a;
b
;;

a;
b;
c
;;

(** [Pexp_while of expression * expression  (** [while E1 do E2 done] *)] *)

while true do
  1
done
;;

(** [Pexp_for of pattern * expression * expression * direction_flag * expression] *)

for x = 0 to 9 do
  1
done
;;

(** [Pexp_constraint of expression * core_type  (** [(E : T)] *)] *)

(1 : int);;

(** [Pexp_coerce of expression * core_type option * core_type] *)

(x :> t2);;
(x : t1 :> t2);;

(** [Pexp_send of expression * label loc  (** [E # m] *)] *)

obj#meth1;;

(** [Pexp_new of Longident.t loc  (** [new M.c] *)] *)

new a;;
new M.a;;

(** [Pexp_setinstvar of label loc * expression  (** [x <- 2] *)] *)

x <- 1;;

(** [Pexp_override of (label loc * expression) list] *)

{<x = 1; y = 2>};;

(** [Pexp_letmodule of string option loc * module_expr * expression] *)

let module M = struct end in
1
;;

(** [Pexp_letexception of extension_constructor * expression] *)

let exception E in
1
;;

(** [Pexp_assert of expression] *)

assert true;;

(** [Pexp_lazy of expression  (** [lazy E] *)] *)

lazy 1;;

(** [Pexp_poly of expression * core_type option] *)

object
  method x = 1
end
;;

object
  method x : int = 1
end
;;

(** [Pexp_object of class_structure  (** [object ... end] *)] *)

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

(** [Pexp_newtype of string loc * expression  (** [fun (type t) -> E] *)] *)

fun (type x) -> 1;;

(** [Pexp_pack of module_expr * package_type option] *)

(module X : x);;
(module X : S with type t1 = int);;

(** [Pexp_open of open_declaration * expression] *)

let open M in
1
;;

(** [Pexp_letop of letop] *)

let* x in
1
;;

let* x and+ y = 2 in
1
;;

(** [Pexp_extension of extension  (** [[%id]] *)] *)

let x = [%ext1];;

(** [Pexp_unreachable  (** [.] *)] *)

match x with . -> 1;;

(** [Pparam_val of arg_label * expression option * pattern] *)

(** [Pparam_newtype of string loc] *)

(** [Pfunction_body of expression] *)

(** [Pfunction_cases of case list * Location.t * attributes] *)

(** [Pconstraint of core_type] *)

(** [Pcoerce of core_type option * core_type] *)

(** [Ptype_abstract] *)

(** [Ptype_variant of constructor_declaration list] *)

(** [Ptype_record of label_declaration list  (** Invariant: non-empty list *)] *)

(** [Ptype_open] *)

(** [Pcstr_tuple of core_type list] *)

(** [Pcstr_record of label_declaration list] *)

(** [Pext_decl of string loc list * constructor_arguments * core_type option] *)

(** [Pext_rebind of Longident.t loc] *)

(** [Pcty_constr of Longident.t loc * core_type list] *)

(** [Pcty_signature of class_signature  (** [object ... end] *)] *)

(** [Pcty_arrow of arg_label * core_type * class_type] *)

(** [Pcty_extension of extension  (** [%id] *)] *)

(** [Pcty_open of open_description * class_type  (** [let open M in CT] *)] *)

(** [Pctf_inherit of class_type  (** [inherit CT] *)] *)

(** [Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)] *)

(** [Pctf_method of (label loc * private_flag * virtual_flag * core_type)] *)

(** [Pctf_constraint of (core_type * core_type)  (** [constraint T1 = T2] *)] *)

(** [Pctf_attribute of attribute  (** [[\@\@\@id]] *)] *)

(** [Pctf_extension of extension  (** [[%%id]] *)] *)

(** [Pcl_constr of Longident.t loc * core_type list] *)

(** [Pcl_structure of class_structure  (** [object ... end] *)] *)

(** [Pcl_fun of arg_label * expression option * pattern * class_expr] *)

(** [Pcl_apply of class_expr * (arg_label * expression) list] *)

(** [Pcl_let of rec_flag * value_binding list * class_expr] *)

(** [Pcl_constraint of class_expr * class_type  (** [(CE : CT)] *)] *)

(** [Pcl_extension of extension  (** [[%id]] *)] *)

(** [Pcl_open of open_description * class_expr  (** [let open M in CE] *)] *)

(** [Pcf_inherit of override_flag * class_expr * string loc option] *)

(** [Pcf_val of (label loc * mutable_flag * class_field_kind)] *)

(** [Pcf_method of (label loc * private_flag * class_field_kind)] *)

(** [Pcf_constraint of (core_type * core_type)  (** [constraint T1 = T2] *)] *)

(** [Pcf_initializer of expression  (** [initializer E] *)] *)

(** [Pcf_attribute of attribute  (** [[\@\@\@id]] *)] *)

(** [Pcf_extension of extension  (** [[%%id]] *)] *)

(** [Pmty_ident of Longident.t loc  (** [Pmty_ident(S)] represents [S] *)] *)

(** [Pmty_signature of signature  (** [sig ... end] *)] *)

(** [Pmty_functor of functor_parameter * module_type] *)

(** [Pmty_with of module_type * with_constraint list  (** [MT with ...] *)] *)

(** [Pmty_typeof of module_expr  (** [module type of ME] *)] *)

(** [Pmty_extension of extension  (** [[%id]] *)] *)

(** [Pmty_alias of Longident.t loc  (** [(module M)] *)] *)

(** [Psig_value of value_description] *)

(** [Psig_type of rec_flag * type_declaration list] *)

(** [Psig_typesubst of type_declaration list] *)

(** [Psig_typext of type_extension  (** [type t1 += ...] *)] *)

(** [Psig_exception of type_exception  (** [exception C of T] *)] *)

(** [Psig_module of module_declaration  (** [module X = M] and [module X : MT] *)] *)

(** [Psig_modsubst of module_substitution  (** [module X := M] *)] *)

(** [Psig_recmodule of module_declaration list] *)

(** [Psig_modtype of module_type_declaration] *)

(** [Psig_modtypesubst of module_type_declaration] *)

(** [Psig_open of open_description  (** [open X] *)] *)

(** [Psig_include of include_description  (** [include MT] *)] *)

(** [Psig_class of class_description list] *)

(** [Psig_class_type of class_type_declaration list] *)

(** [Psig_attribute of attribute  (** [[\@\@\@id]] *)] *)

(** [Psig_extension of extension * attributes  (** [[%%id]] *)] *)

(** [Pwith_type of Longident.t loc * type_declaration] *)

(** [Pwith_module of Longident.t loc * Longident.t loc] *)

(** [Pwith_modtype of Longident.t loc * module_type] *)

(** [Pwith_modtypesubst of Longident.t loc * module_type] *)

(** [Pwith_typesubst of Longident.t loc * type_declaration] *)

(** [Pwith_modsubst of Longident.t loc * Longident.t loc] *)

(** [Pmod_ident of Longident.t loc  (** [X] *)] *)

(** [Pmod_structure of structure  (** [struct ... end] *)] *)

(** [Pmod_functor of functor_parameter * module_expr] *)

(** [Pmod_apply of module_expr * module_expr (** [ME1(ME2)] *)] *)

(** [Pmod_apply_unit of module_expr (** [ME1()] *)] *)

(** [Pmod_constraint of module_expr * module_type  (** [(ME : MT)] *)] *)

(** [Pmod_unpack of expression  (** [(val E)] *)] *)

(** [Pmod_extension of extension  (** [[%id]] *)] *)

(** [Pstr_eval of expression * attributes  (** [E] *)] *)

(** [Pstr_value of rec_flag * value_binding list] *)

(** [Pstr_primitive of value_description] *)

(** [Pstr_type of rec_flag * type_declaration list] *)

(** [Pstr_typext of type_extension  (** [type t1 += ...] *)] *)

(** [Pstr_exception of type_exception] *)

(** [Pstr_module of module_binding  (** [module X = ME] *)] *)

(** [Pstr_recmodule of module_binding list] *)

(** [Pstr_modtype of module_type_declaration  (** [module type S = MT] *)] *)

(** [Pstr_open of open_declaration  (** [open X] *)] *)

(** [Pstr_class of class_declaration list] *)

(** [Pstr_class_type of class_type_declaration list] *)

(** [Pstr_include of include_declaration  (** [include ME] *)] *)

(** [Pstr_attribute of attribute  (** [[\@\@\@id]] *)] *)

(** [Pstr_extension of extension * attributes  (** [[%%id]] *)] *)

(** [Pvc_constraint of {] *)

(** [Pvc_coercion of {ground:core_type option; coercion:core_type }] *)

(** [Ptop_def of structure] *)

(** [Ptop_dir of toplevel_directive  (** [#use], [#load] ... *)] *)

(** [Pdir_string of string] *)

(** [Pdir_int of string * char option] *)

(** [Pdir_ident of Longident.t] *)

(** [Pdir_bool of bool] *)
