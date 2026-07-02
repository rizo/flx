open struct
  module Target_ast = Astlib.Ast_504
  module Asttypes = Target_ast.Asttypes
  module Longident = Target_ast.Longident
  module Parsetree = Target_ast.Parsetree
end

open Asttypes
open Format
open Location
open Parsetree

let pp_list pp ppf = function
  | [] -> fprintf ppf "()"
  | x :: xs ->
    fprintf ppf "@[<1>(";
    pp ppf x;
    List.iter (fun x -> fprintf ppf "@ %a" pp x) xs;
    fprintf ppf ")@]"

let pp_option pp ppf = function
  | None -> fprintf ppf "None"
  | Some x -> fprintf ppf "@[<1>(Some@ %a)@]" pp x

let list i pp ppf xs = pp_list (pp i) ppf xs
let option i pp ppf x = pp_option (pp i) ppf x
let pp_string ppf s = fprintf ppf "%S" s
let pp_bool ppf b = fprintf ppf "%b" b
let pp_char ppf c = fprintf ppf "%C" c

(* let pp_position ppf pos = *)
(*   if pos.pos_lnum <= 0 then fprintf ppf "%d" pos.pos_cnum *)
(*   else fprintf ppf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) *)

let rec pp_longident ppf = function
  | Longident.Lident s -> fprintf ppf "@[<1>(Lident@ %S)@]" s
  | Longident.Ldot (li, s) ->
    fprintf ppf "@[<1>(Ldot@ %a@ %S)@]" pp_longident li.txt s.txt
  | Longident.Lapply (li1, li2) ->
    fprintf ppf "@[<1>(Lapply@ %a@ %a)@]" pp_longident li1.txt pp_longident
      li2.txt

(* let pp_loc pp ppf x = *)
(*   fprintf ppf "@[<1>((txt@ %a)@ (loc@ %a))@]" pp x.txt pp_location x.loc *)

let pp_loc pp ppf x = fprintf ppf "%a" pp x.txt

let pp_string_option ppf = function
  | None -> fprintf ppf "None"
  | Some s -> fprintf ppf "@[<1>(Some@ %S)@]" s

let pp_char_option ppf x = pp_option pp_char ppf x

let rec_flag ppf = function
  | Nonrecursive -> fprintf ppf "Nonrecursive"
  | Recursive -> fprintf ppf "Recursive"

let direction_flag ppf = function
  | Upto -> fprintf ppf "Upto"
  | Downto -> fprintf ppf "Downto"

let private_flag ppf = function
  | Private -> fprintf ppf "Private"
  | Public -> fprintf ppf "Public"

let mutable_flag ppf = function
  | Immutable -> fprintf ppf "Immutable"
  | Mutable -> fprintf ppf "Mutable"

let virtual_flag ppf = function
  | Virtual -> fprintf ppf "Virtual"
  | Concrete -> fprintf ppf "Concrete"

let override_flag ppf = function
  | Override -> fprintf ppf "Override"
  | Fresh -> fprintf ppf "Fresh"

let closed_flag ppf = function
  | Closed -> fprintf ppf "Closed"
  | Open -> fprintf ppf "Open"

let arg_label ppf = function
  | Nolabel -> fprintf ppf "Nolabel"
  | Labelled s -> fprintf ppf "@[<1>(Labelled@ %S)@]" s
  | Optional s -> fprintf ppf "@[<1>(Optional@ %S)@]" s

let variance ppf = function
  | Covariant -> fprintf ppf "Covariant"
  | Contravariant -> fprintf ppf "Contravariant"
  | NoVariance -> fprintf ppf "NoVariance"
  | Bivariant -> fprintf ppf "Bivariant"

let injectivity ppf = function
  | Injective -> fprintf ppf "Injective"
  | NoInjectivity -> fprintf ppf "NoInjectivity"

let type_variance ppf (v, i) =
  fprintf ppf "@[<1>(%a@ %a)@]" variance v injectivity i

let rec constant i ppf x =
  fprintf ppf "@[<1>((pconst_desc@ %a))@]" (constant_desc i) x.pconst_desc

and constant_desc _ ppf = function
  | Pconst_integer (s, suffix) ->
    fprintf ppf "@[<1>(Pconst_integer@ %S@ %a)@]" s pp_char_option suffix
  | Pconst_char c -> fprintf ppf "@[<1>(Pconst_char@ %a)@]" pp_char c
  | Pconst_string (s, _loc, delim) ->
    fprintf ppf "@[<1>(Pconst_string@ %S@ %a)@]" s (pp_option pp_string) delim
  | Pconst_float (s, suffix) ->
    fprintf ppf "@[<1>(Pconst_float@ %S@ %a)@]" s pp_char_option suffix

and core_type i ppf x =
  fprintf ppf "@[<1>((ptyp_desc@ %a)@ (ptyp_attributes@ %a))@]"
    (core_type_desc i) x.ptyp_desc (attributes i) x.ptyp_attributes

and core_type_desc i ppf = function
  | Ptyp_any -> fprintf ppf "Ptyp_any"
  | Ptyp_var s -> fprintf ppf "@[<1>(Ptyp_var@ %S)@]" s
  | Ptyp_arrow (lbl, t1, t2) ->
    fprintf ppf "@[<1>(Ptyp_arrow@ %a@ %a@ %a)@]" arg_label lbl (core_type i) t1
      (core_type i) t2
  (* FIXME: Print label *)
  | Ptyp_tuple ts ->
    fprintf ppf "@[<1>(Ptyp_tuple@ %a)@]" (list i core_type) (List.map snd ts)
  | Ptyp_constr (li, ts) ->
    fprintf ppf "@[<1>(Ptyp_constr@ %a@ %a)@]" (pp_loc pp_longident) li
      (list i core_type) ts
  | Ptyp_object (fields, closed) ->
    fprintf ppf "@[<1>(Ptyp_object@ %a@ %a)@]" (list i object_field) fields
      closed_flag closed
  | Ptyp_class (li, ts) ->
    fprintf ppf "@[<1>(Ptyp_class@ %a@ %a)@]" (pp_loc pp_longident) li
      (list i core_type) ts
  | Ptyp_alias (t, name) ->
    fprintf ppf "@[<1>(Ptyp_alias@ %a@ %a)@]" (core_type i) t (pp_loc pp_string)
      name
  | Ptyp_variant (fields, closed, labels) ->
    fprintf ppf "@[<1>(Ptyp_variant@ %a@ %a@ %a)@]" (list i row_field) fields
      closed_flag closed
      (pp_option (pp_list pp_string))
      labels
  | Ptyp_poly (vars, t) ->
    fprintf ppf "@[<1>(Ptyp_poly@ %a@ %a)@]"
      (pp_list (pp_loc pp_string))
      vars (core_type i) t
  | Ptyp_package p -> fprintf ppf "@[<1>(Ptyp_package@ %a)@]" (package_type i) p
  | Ptyp_open (li, t) ->
    fprintf ppf "@[<1>(Ptyp_open@ %a@ %a)@]" (pp_loc pp_longident) li
      (core_type i) t
  | Ptyp_extension ext ->
    fprintf ppf "@[<1>(Ptyp_extension@ %a)@]" (extension i) ext

and package_type i ppf (pt : Parsetree.package_type) =
  fprintf ppf "@[<1>(%a@ %a)@]" (pp_loc pp_longident) pt.ppt_path
    (pp_list (longident_x_core_type i))
    pt.ppt_cstrs

and row_field i ppf x =
  fprintf ppf "@[<1>((prf_desc@ %a)@ (prf_attributes@ %a))@]" (row_field_desc i)
    x.prf_desc (attributes i) x.prf_attributes

and row_field_desc i ppf = function
  | Rtag (label, constant, ts) ->
    fprintf ppf "@[<1>(Rtag@ %a@ %a@ %a)@]" (pp_loc pp_string) label pp_bool
      constant (list i core_type) ts
  | Rinherit t -> fprintf ppf "@[<1>(Rinherit@ %a)@]" (core_type i) t

and object_field i ppf x =
  fprintf ppf "@[<1>((pof_desc@ %a)@ (pof_attributes@ %a))@]"
    (object_field_desc i) x.pof_desc (attributes i) x.pof_attributes

and object_field_desc i ppf = function
  | Otag (label, t) ->
    fprintf ppf "@[<1>(Otag@ %a@ %a)@]" (pp_loc pp_string) label (core_type i) t
  | Oinherit t -> fprintf ppf "@[<1>(Oinherit@ %a)@]" (core_type i) t

and pattern i ppf x =
  fprintf ppf "@[<1>((ppat_desc@ %a)@ (ppat_attributes@ %a))@]" (pattern_desc i)
    x.ppat_desc (attributes i) x.ppat_attributes

and pattern_desc i ppf = function
  | Ppat_any -> fprintf ppf "Ppat_any"
  | Ppat_var name -> fprintf ppf "@[<1>(Ppat_var@ %a)@]" (pp_loc pp_string) name
  | Ppat_alias (p, name) ->
    fprintf ppf "@[<1>(Ppat_alias@ %a@ %a)@]" (pattern i) p (pp_loc pp_string)
      name
  | Ppat_constant c -> fprintf ppf "@[<1>(Ppat_constant@ %a)@]" (constant i) c
  | Ppat_interval (c1, c2) ->
    fprintf ppf "@[<1>(Ppat_interval@ %a@ %a)@]" (constant i) c1 (constant i) c2
  (* TODO: closed; labels *)
  | Ppat_tuple (ps, _closed) ->
    fprintf ppf "@[<1>(Ppat_tuple@ %a)@]" (list i pattern) (List.map snd ps)
  | Ppat_construct (li, arg) ->
    fprintf ppf "@[<1>(Ppat_construct@ %a@ %a)@]" (pp_loc pp_longident) li
      (pp_option (constructor_pattern i))
      arg
  | Ppat_variant (label, arg) ->
    fprintf ppf "@[<1>(Ppat_variant@ %S@ %a)@]" label (option i pattern) arg
  | Ppat_record (fields, closed) ->
    fprintf ppf "@[<1>(Ppat_record@ %a@ %a)@]"
      (pp_list (longident_x_pattern i))
      fields closed_flag closed
  | Ppat_array ps -> fprintf ppf "@[<1>(Ppat_array@ %a)@]" (list i pattern) ps
  | Ppat_or (p1, p2) ->
    fprintf ppf "@[<1>(Ppat_or@ %a@ %a)@]" (pattern i) p1 (pattern i) p2
  | Ppat_constraint (p, t) ->
    fprintf ppf "@[<1>(Ppat_constraint@ %a@ %a)@]" (pattern i) p (core_type i) t
  | Ppat_type li ->
    fprintf ppf "@[<1>(Ppat_type@ %a)@]" (pp_loc pp_longident) li
  | Ppat_lazy p -> fprintf ppf "@[<1>(Ppat_lazy@ %a)@]" (pattern i) p
  | Ppat_unpack name ->
    fprintf ppf "@[<1>(Ppat_unpack@ %a)@]" (pp_loc pp_string_option) name
  | Ppat_exception p -> fprintf ppf "@[<1>(Ppat_exception@ %a)@]" (pattern i) p
  | Ppat_effect (p1, p2) ->
    fprintf ppf "@[<1>(Ppat_effect@ %a@ %a)@]" (pattern i) p1 (pattern i) p2
  | Ppat_extension ext ->
    fprintf ppf "@[<1>(Ppat_extension@ %a)@]" (extension i) ext
  | Ppat_open (li, p) ->
    fprintf ppf "@[<1>(Ppat_open@ %a@ %a)@]" (pp_loc pp_longident) li
      (pattern i) p

and constructor_pattern i ppf (vars, p) =
  fprintf ppf "@[<1>(%a@ %a)@]" (pp_list (pp_loc pp_string)) vars (pattern i) p

and expression i ppf x =
  fprintf ppf "@[<1>((pexp_desc@ %a)@ (pexp_attributes@ %a))@]"
    (expression_desc i) x.pexp_desc (attributes i) x.pexp_attributes

and expression_desc i ppf = function
  | Pexp_ident li ->
    fprintf ppf "@[<1>(Pexp_ident@ %a)@]" (pp_loc pp_longident) li
  | Pexp_constant c -> fprintf ppf "@[<1>(Pexp_constant@ %a)@]" (constant i) c
  | Pexp_let (rec_, bindings, body) ->
    fprintf ppf "@[<1>(Pexp_let@ %a@ %a@ %a)@]" rec_flag rec_
      (list i value_binding) bindings (expression i) body
  | Pexp_function (params, constraint_, body) ->
    fprintf ppf "@[<1>(Pexp_function@ %a@ %a@ %a)@]" (list i function_param)
      params (option i type_constraint) constraint_ (function_body i) body
  | Pexp_apply (f, args) ->
    fprintf ppf "@[<1>(Pexp_apply@ %a@ %a)@]" (expression i) f
      (pp_list (arg_label_x_expression i))
      args
  | Pexp_match (e, cases) ->
    fprintf ppf "@[<1>(Pexp_match@ %a@ %a)@]" (expression i) e (list i case)
      cases
  | Pexp_try (e, cases) ->
    fprintf ppf "@[<1>(Pexp_try@ %a@ %a)@]" (expression i) e (list i case) cases
  (* TODO: labels *)
  | Pexp_tuple es ->
    fprintf ppf "@[<1>(Pexp_tuple@ %a)@]" (list i expression) (List.map snd es)
  | Pexp_construct (li, arg) ->
    fprintf ppf "@[<1>(Pexp_construct@ %a@ %a)@]" (pp_loc pp_longident) li
      (option i expression) arg
  | Pexp_variant (label, arg) ->
    fprintf ppf "@[<1>(Pexp_variant@ %S@ %a)@]" label (option i expression) arg
  | Pexp_record (fields, base) ->
    fprintf ppf "@[<1>(Pexp_record@ %a@ %a)@]"
      (pp_list (longident_x_expression i))
      fields (option i expression) base
  | Pexp_field (e, li) ->
    fprintf ppf "@[<1>(Pexp_field@ %a@ %a)@]" (expression i) e
      (pp_loc pp_longident) li
  | Pexp_setfield (e1, li, e2) ->
    fprintf ppf "@[<1>(Pexp_setfield@ %a@ %a@ %a)@]" (expression i) e1
      (pp_loc pp_longident) li (expression i) e2
  | Pexp_array es ->
    fprintf ppf "@[<1>(Pexp_array@ %a)@]" (list i expression) es
  | Pexp_ifthenelse (cond, then_, else_) ->
    fprintf ppf "@[<1>(Pexp_ifthenelse@ %a@ %a@ %a)@]" (expression i) cond
      (expression i) then_ (option i expression) else_
  | Pexp_sequence (e1, e2) ->
    fprintf ppf "@[<1>(Pexp_sequence@ %a@ %a)@]" (expression i) e1
      (expression i) e2
  | Pexp_while (cond, body) ->
    fprintf ppf "@[<1>(Pexp_while@ %a@ %a)@]" (expression i) cond (expression i)
      body
  | Pexp_for (p, start, stop, direction, body) ->
    fprintf ppf "@[<1>(Pexp_for@ %a@ %a@ %a@ %a@ %a)@]" (pattern i) p
      (expression i) start (expression i) stop direction_flag direction
      (expression i) body
  | Pexp_constraint (e, t) ->
    fprintf ppf "@[<1>(Pexp_constraint@ %a@ %a)@]" (expression i) e
      (core_type i) t
  | Pexp_coerce (e, from_, to_) ->
    fprintf ppf "@[<1>(Pexp_coerce@ %a@ %a@ %a)@]" (expression i) e
      (option i core_type) from_ (core_type i) to_
  | Pexp_send (e, label) ->
    fprintf ppf "@[<1>(Pexp_send@ %a@ %a)@]" (expression i) e (pp_loc pp_string)
      label
  | Pexp_new li -> fprintf ppf "@[<1>(Pexp_new@ %a)@]" (pp_loc pp_longident) li
  | Pexp_setinstvar (label, e) ->
    fprintf ppf "@[<1>(Pexp_setinstvar@ %a@ %a)@]" (pp_loc pp_string) label
      (expression i) e
  | Pexp_override fields ->
    fprintf ppf "@[<1>(Pexp_override@ %a)@]"
      (pp_list (label_x_expression i))
      fields
  | Pexp_letmodule (name, module_, body) ->
    fprintf ppf "@[<1>(Pexp_letmodule@ %a@ %a@ %a)@]" (pp_loc pp_string_option)
      name (module_expr i) module_ (expression i) body
  | Pexp_letexception (constructor, body) ->
    fprintf ppf "@[<1>(Pexp_letexception@ %a@ %a)@]" (extension_constructor i)
      constructor (expression i) body
  | Pexp_assert e -> fprintf ppf "@[<1>(Pexp_assert@ %a)@]" (expression i) e
  | Pexp_lazy e -> fprintf ppf "@[<1>(Pexp_lazy@ %a)@]" (expression i) e
  | Pexp_poly (e, constraint_) ->
    fprintf ppf "@[<1>(Pexp_poly@ %a@ %a)@]" (expression i) e
      (option i core_type) constraint_
  | Pexp_object structure ->
    fprintf ppf "@[<1>(Pexp_object@ %a)@]" (class_structure i) structure
  | Pexp_newtype (name, body) ->
    fprintf ppf "@[<1>(Pexp_newtype@ %a@ %a)@]" (pp_loc pp_string) name
      (expression i) body
  (* TODO: pack type *)
  | Pexp_pack (module_, _) ->
    fprintf ppf "@[<1>(Pexp_pack@ %a)@]" (module_expr i) module_
  | Pexp_open (open_, body) ->
    fprintf ppf "@[<1>(Pexp_open@ %a@ %a)@]" (open_declaration i) open_
      (expression i) body
  | Pexp_letop letop_ -> fprintf ppf "@[<1>(Pexp_letop@ %a)@]" (letop i) letop_
  | Pexp_extension ext ->
    fprintf ppf "@[<1>(Pexp_extension@ %a)@]" (extension i) ext
  | Pexp_unreachable -> fprintf ppf "Pexp_unreachable"

and case i ppf x =
  fprintf ppf "@[<1>((pc_lhs@ %a)@ (pc_guard@ %a)@ (pc_rhs@ %a))@]" (pattern i)
    x.pc_lhs (option i expression) x.pc_guard (expression i) x.pc_rhs

and letop i ppf x =
  fprintf ppf "@[<1>((let_@ %a)@ (ands@ %a)@ (body@ %a))@]" (binding_op i)
    x.let_ (list i binding_op) x.ands (expression i) x.body

and binding_op i ppf x =
  fprintf ppf "@[<1>((pbop_op@ %a)@ (pbop_pat@ %a)@ (pbop_exp@ %a))@]"
    (pp_loc pp_string) x.pbop_op (pattern i) x.pbop_pat (expression i)
    x.pbop_exp

and function_param i ppf x =
  fprintf ppf "@[<1>(pparam_desc@ %a)@]" (function_param_desc i) x.pparam_desc

and function_param_desc i ppf = function
  | Pparam_val (label, default, p) ->
    fprintf ppf "@[<1>(Pparam_val@ %a@ %a@ %a)@]" arg_label label
      (option i expression) default (pattern i) p
  | Pparam_newtype name ->
    fprintf ppf "@[<1>(Pparam_newtype@ %a)@]" (pp_loc pp_string) name

and function_body i ppf = function
  | Pfunction_body e ->
    fprintf ppf "@[<1>(Pfunction_body@ %a)@]" (expression i) e
  | Pfunction_cases (cases, _loc, attrs) ->
    fprintf ppf "@[<1>(Pfunction_cases@ %a@ %a)@]" (list i case) cases
      (attributes i) attrs

and type_constraint i ppf = function
  | Pconstraint t -> fprintf ppf "@[<1>(Pconstraint@ %a)@]" (core_type i) t
  | Pcoerce (from_, to_) ->
    fprintf ppf "@[<1>(Pcoerce@ %a@ %a)@]" (option i core_type) from_
      (core_type i) to_

and value_description i ppf x =
  fprintf ppf
    "@[<1>((pval_name@ %a)@ (pval_type@ %a)@ (pval_prim@ %a)@ \
     (pval_attributes@ %a))@]"
    (pp_loc pp_string) x.pval_name (core_type i) x.pval_type (pp_list pp_string)
    x.pval_prim (attributes i) x.pval_attributes

and type_declaration i ppf x =
  fprintf ppf
    "@[<1>((ptype_name@ %a)@ (ptype_params@ %a)@ (ptype_cstrs@ %a)@ \
     (ptype_kind@ %a)@ (ptype_private@ %a)@ (ptype_manifest@ %a)@ \
     (ptype_attributes@ %a))@]"
    (pp_loc pp_string) x.ptype_name (list i type_parameter) x.ptype_params
    (pp_list (core_type_x_core_type i))
    x.ptype_cstrs (type_kind i) x.ptype_kind private_flag x.ptype_private
    (option i core_type) x.ptype_manifest (attributes i) x.ptype_attributes

and type_parameter i ppf (t, variance) =
  fprintf ppf "@[<1>(%a@ %a)@]" (core_type i) t type_variance variance

and type_kind i ppf = function
  | Ptype_abstract -> fprintf ppf "Ptype_abstract"
  | Ptype_variant constructors ->
    fprintf ppf "@[<1>(Ptype_variant@ %a)@]" (list i constructor_decl)
      constructors
  | Ptype_record labels ->
    fprintf ppf "@[<1>(Ptype_record@ %a)@]" (list i label_decl) labels
  | Ptype_open -> fprintf ppf "Ptype_open"

and label_decl i ppf x =
  fprintf ppf
    "@[<1>((pld_name@ %a)@ (pld_mutable@ %a)@ (pld_type@ %a)@ (pld_attributes@ \
     %a))@]"
    (pp_loc pp_string) x.pld_name mutable_flag x.pld_mutable (core_type i)
    x.pld_type (attributes i) x.pld_attributes

and constructor_decl i ppf x =
  fprintf ppf
    "@[<1>((pcd_name@ %a)@ (pcd_vars@ %a)@ (pcd_args@ %a)@ (pcd_res@ %a)@ \
     (pcd_attributes@ %a))@]"
    (pp_loc pp_string) x.pcd_name
    (pp_list (pp_loc pp_string))
    x.pcd_vars (constructor_arguments i) x.pcd_args (option i core_type)
    x.pcd_res (attributes i) x.pcd_attributes

and constructor_arguments i ppf = function
  | Pcstr_tuple ts ->
    fprintf ppf "@[<1>(Pcstr_tuple@ %a)@]" (list i core_type) ts
  | Pcstr_record labels ->
    fprintf ppf "@[<1>(Pcstr_record@ %a)@]" (list i label_decl) labels

and type_extension i ppf x =
  fprintf ppf
    "@[<1>((ptyext_path@ %a)@ (ptyext_params@ %a)@ (ptyext_constructors@ %a)@ \
     (ptyext_private@ %a)@ (ptyext_attributes@ %a))@]"
    (pp_loc pp_longident) x.ptyext_path (list i type_parameter) x.ptyext_params
    (list i extension_constructor)
    x.ptyext_constructors private_flag x.ptyext_private (attributes i)
    x.ptyext_attributes

and extension_constructor i ppf x =
  fprintf ppf "@[<1>((pext_name@ %a)@ (pext_kind@ %a)@ (pext_attributes@ %a))@]"
    (pp_loc pp_string) x.pext_name
    (extension_constructor_kind i)
    x.pext_kind (attributes i) x.pext_attributes

and type_exception i ppf x =
  fprintf ppf "@[<1>((ptyexn_constructor@ %a)@ (ptyexn_attributes@ %a))@]"
    (extension_constructor i) x.ptyexn_constructor (attributes i)
    x.ptyexn_attributes

and extension_constructor_kind i ppf = function
  | Pext_decl (vars, args, res) ->
    fprintf ppf "@[<1>(Pext_decl@ %a@ %a@ %a)@]"
      (pp_list (pp_loc pp_string))
      vars (constructor_arguments i) args (option i core_type) res
  | Pext_rebind li ->
    fprintf ppf "@[<1>(Pext_rebind@ %a)@]" (pp_loc pp_longident) li

and class_type i ppf x =
  fprintf ppf "@[<1>((pcty_desc@ %a)@ (pcty_attributes@ %a))@]"
    (class_type_desc i) x.pcty_desc (attributes i) x.pcty_attributes

and class_type_desc i ppf = function
  | Pcty_constr (li, ts) ->
    fprintf ppf "@[<1>(Pcty_constr@ %a@ %a)@]" (pp_loc pp_longident) li
      (list i core_type) ts
  | Pcty_signature signature ->
    fprintf ppf "@[<1>(Pcty_signature@ %a)@]" (class_signature i) signature
  | Pcty_arrow (label, t, result) ->
    fprintf ppf "@[<1>(Pcty_arrow@ %a@ %a@ %a)@]" arg_label label (core_type i)
      t (class_type i) result
  | Pcty_extension ext ->
    fprintf ppf "@[<1>(Pcty_extension@ %a)@]" (extension i) ext
  | Pcty_open (open_, t) ->
    fprintf ppf "@[<1>(Pcty_open@ %a@ %a)@]" (open_description i) open_
      (class_type i) t

and class_signature i ppf x =
  fprintf ppf "@[<1>((pcsig_self@ %a)@ (pcsig_fields@ %a))@]" (core_type i)
    x.pcsig_self (list i class_type_field) x.pcsig_fields

and class_type_field i ppf x =
  fprintf ppf "@[<1>((pctf_desc@ %a)@ (pctf_attributes@ %a))@]"
    (class_type_field_desc i) x.pctf_desc (attributes i) x.pctf_attributes

and class_type_field_desc i ppf = function
  | Pctf_inherit t -> fprintf ppf "@[<1>(Pctf_inherit@ %a)@]" (class_type i) t
  | Pctf_val (label, mutable_, virtual_, t) ->
    fprintf ppf "@[<1>(Pctf_val@ %a@ %a@ %a@ %a)@]" (pp_loc pp_string) label
      mutable_flag mutable_ virtual_flag virtual_ (core_type i) t
  | Pctf_method (label, private_, virtual_, t) ->
    fprintf ppf "@[<1>(Pctf_method@ %a@ %a@ %a@ %a)@]" (pp_loc pp_string) label
      private_flag private_ virtual_flag virtual_ (core_type i) t
  | Pctf_constraint (t1, t2) ->
    fprintf ppf "@[<1>(Pctf_constraint@ %a@ %a)@]" (core_type i) t1
      (core_type i) t2
  | Pctf_attribute attr ->
    fprintf ppf "@[<1>(Pctf_attribute@ %a)@]" (attribute i) attr
  | Pctf_extension ext ->
    fprintf ppf "@[<1>(Pctf_extension@ %a)@]" (extension i) ext

and class_description i ppf x = class_type_infos i ppf x
and class_type_declaration i ppf x = class_type_infos i ppf x

and class_type_infos i ppf x =
  fprintf ppf
    "@[<1>((pci_virt@ %a)@ (pci_params@ %a)@ (pci_name@ %a)@ (pci_expr@ %a)@ \
     (pci_attributes@ %a))@]"
    virtual_flag x.pci_virt (list i type_parameter) x.pci_params
    (pp_loc pp_string) x.pci_name (class_type i) x.pci_expr (attributes i)
    x.pci_attributes

and class_expr i ppf x =
  fprintf ppf "@[<1>((pcl_desc@ %a)@ (pcl_attributes@ %a))@]"
    (class_expr_desc i) x.pcl_desc (attributes i) x.pcl_attributes

and class_expr_desc i ppf = function
  | Pcl_constr (li, ts) ->
    fprintf ppf "@[<1>(Pcl_constr@ %a@ %a)@]" (pp_loc pp_longident) li
      (list i core_type) ts
  | Pcl_structure structure ->
    fprintf ppf "@[<1>(Pcl_structure@ %a)@]" (class_structure i) structure
  | Pcl_fun (label, default, p, body) ->
    fprintf ppf "@[<1>(Pcl_fun@ %a@ %a@ %a@ %a)@]" arg_label label
      (option i expression) default (pattern i) p (class_expr i) body
  | Pcl_apply (class_, args) ->
    fprintf ppf "@[<1>(Pcl_apply@ %a@ %a)@]" (class_expr i) class_
      (pp_list (arg_label_x_expression i))
      args
  | Pcl_let (rec_, bindings, body) ->
    fprintf ppf "@[<1>(Pcl_let@ %a@ %a@ %a)@]" rec_flag rec_
      (list i value_binding) bindings (class_expr i) body
  | Pcl_constraint (class_, t) ->
    fprintf ppf "@[<1>(Pcl_constraint@ %a@ %a)@]" (class_expr i) class_
      (class_type i) t
  | Pcl_extension ext ->
    fprintf ppf "@[<1>(Pcl_extension@ %a)@]" (extension i) ext
  | Pcl_open (open_, body) ->
    fprintf ppf "@[<1>(Pcl_open@ %a@ %a)@]" (open_description i) open_
      (class_expr i) body

and class_structure i ppf x =
  fprintf ppf "@[<1>((pcstr_self@ %a)@ (pcstr_fields@ %a))@]" (pattern i)
    x.pcstr_self (list i class_field) x.pcstr_fields

and class_field i ppf x =
  fprintf ppf "@[<1>((pcf_desc@ %a)@ (pcf_attributes@ %a))@]"
    (class_field_desc i) x.pcf_desc (attributes i) x.pcf_attributes

and class_field_desc i ppf = function
  | Pcf_inherit (override_, class_, alias) ->
    fprintf ppf "@[<1>(Pcf_inherit@ %a@ %a@ %a)@]" override_flag override_
      (class_expr i) class_
      (pp_option (pp_loc pp_string))
      alias
  | Pcf_val (label, mutable_, kind) ->
    fprintf ppf "@[<1>(Pcf_val@ %a@ %a@ %a)@]" (pp_loc pp_string) label
      mutable_flag mutable_ (class_field_kind i) kind
  | Pcf_method (label, private_, kind) ->
    fprintf ppf "@[<1>(Pcf_method@ %a@ %a@ %a)@]" (pp_loc pp_string) label
      private_flag private_ (class_field_kind i) kind
  | Pcf_constraint (t1, t2) ->
    fprintf ppf "@[<1>(Pcf_constraint@ %a@ %a)@]" (core_type i) t1 (core_type i)
      t2
  | Pcf_initializer e ->
    fprintf ppf "@[<1>(Pcf_initializer@ %a)@]" (expression i) e
  | Pcf_attribute attr ->
    fprintf ppf "@[<1>(Pcf_attribute@ %a)@]" (attribute i) attr
  | Pcf_extension ext ->
    fprintf ppf "@[<1>(Pcf_extension@ %a)@]" (extension i) ext

and class_field_kind i ppf = function
  | Cfk_virtual t -> fprintf ppf "@[<1>(Cfk_virtual@ %a)@]" (core_type i) t
  | Cfk_concrete (override_, e) ->
    fprintf ppf "@[<1>(Cfk_concrete@ %a@ %a)@]" override_flag override_
      (expression i) e

and class_declaration i ppf x =
  fprintf ppf
    "@[<1>((pci_virt@ %a)@ (pci_params@ %a)@ (pci_name@ %a)@ (pci_expr@ %a)@ \
     (pci_attributes@ %a))@]"
    virtual_flag x.pci_virt (list i type_parameter) x.pci_params
    (pp_loc pp_string) x.pci_name (class_expr i) x.pci_expr (attributes i)
    x.pci_attributes

and module_type i ppf x =
  fprintf ppf "@[<1>((pmty_desc@ %a)@ (pmty_attributes@ %a))@]"
    (module_type_desc i) x.pmty_desc (attributes i) x.pmty_attributes

and module_type_desc i ppf = function
  | Pmty_ident li ->
    fprintf ppf "@[<1>(Pmty_ident@ %a)@]" (pp_loc pp_longident) li
  | Pmty_signature sg ->
    fprintf ppf "@[<1>(Pmty_signature@ %a)@]" (signature i) sg
  | Pmty_functor (param, result) ->
    fprintf ppf "@[<1>(Pmty_functor@ %a@ %a)@]" (functor_parameter i) param
      (module_type i) result
  | Pmty_with (t, constraints) ->
    fprintf ppf "@[<1>(Pmty_with@ %a@ %a)@]" (module_type i) t
      (list i with_constraint) constraints
  | Pmty_typeof module_ ->
    fprintf ppf "@[<1>(Pmty_typeof@ %a)@]" (module_expr i) module_
  | Pmty_extension ext ->
    fprintf ppf "@[<1>(Pmty_extension@ %a)@]" (extension i) ext
  | Pmty_alias li ->
    fprintf ppf "@[<1>(Pmty_alias@ %a)@]" (pp_loc pp_longident) li

and functor_parameter i ppf = function
  | Unit -> fprintf ppf "Unit"
  | Named (name, t) ->
    fprintf ppf "@[<1>(Named@ %a@ %a)@]" (pp_loc pp_string_option) name
      (module_type i) t

and signature i ppf x = list i signature_item ppf x

and signature_item i ppf x =
  fprintf ppf "@[<1>(psig_desc@ %a)@]" (signature_item_desc i) x.psig_desc

and signature_item_desc i ppf = function
  | Psig_value value ->
    fprintf ppf "@[<1>(Psig_value@ %a)@]" (value_description i) value
  | Psig_type (rec_, types) ->
    fprintf ppf "@[<1>(Psig_type@ %a@ %a)@]" rec_flag rec_
      (list i type_declaration) types
  | Psig_typesubst types ->
    fprintf ppf "@[<1>(Psig_typesubst@ %a)@]" (list i type_declaration) types
  | Psig_typext ext ->
    fprintf ppf "@[<1>(Psig_typext@ %a)@]" (type_extension i) ext
  | Psig_exception exn ->
    fprintf ppf "@[<1>(Psig_exception@ %a)@]" (type_exception i) exn
  | Psig_module module_ ->
    fprintf ppf "@[<1>(Psig_module@ %a)@]" (module_declaration i) module_
  | Psig_modsubst subst ->
    fprintf ppf "@[<1>(Psig_modsubst@ %a)@]" (module_substitution i) subst
  | Psig_recmodule modules ->
    fprintf ppf "@[<1>(Psig_recmodule@ %a)@]"
      (list i module_declaration)
      modules
  | Psig_modtype decl ->
    fprintf ppf "@[<1>(Psig_modtype@ %a)@]" (module_type_declaration i) decl
  | Psig_modtypesubst decl ->
    fprintf ppf "@[<1>(Psig_modtypesubst@ %a)@]"
      (module_type_declaration i)
      decl
  | Psig_open open_ ->
    fprintf ppf "@[<1>(Psig_open@ %a)@]" (open_description i) open_
  | Psig_include include_ ->
    fprintf ppf "@[<1>(Psig_include@ %a)@]" (include_description i) include_
  | Psig_class classes ->
    fprintf ppf "@[<1>(Psig_class@ %a)@]" (list i class_description) classes
  | Psig_class_type classes ->
    fprintf ppf "@[<1>(Psig_class_type@ %a)@]"
      (list i class_type_declaration)
      classes
  | Psig_attribute attr ->
    fprintf ppf "@[<1>(Psig_attribute@ %a)@]" (attribute i) attr
  | Psig_extension (ext, attrs) ->
    fprintf ppf "@[<1>(Psig_extension@ %a@ %a)@]" (extension i) ext
      (attributes i) attrs

and module_declaration i ppf x =
  fprintf ppf "@[<1>((pmd_name@ %a)@ (pmd_type@ %a)@ (pmd_attributes@ %a))@]"
    (pp_loc pp_string_option) x.pmd_name (module_type i) x.pmd_type
    (attributes i) x.pmd_attributes

and module_substitution i ppf x =
  fprintf ppf
    "@[<1>((pms_name@ %a)@ (pms_manifest@ %a)@ (pms_attributes@ %a))@]"
    (pp_loc pp_string) x.pms_name (pp_loc pp_longident) x.pms_manifest
    (attributes i) x.pms_attributes

and module_type_declaration i ppf x =
  fprintf ppf "@[<1>((pmtd_name@ %a)@ (pmtd_type@ %a)@ (pmtd_attributes@ %a))@]"
    (pp_loc pp_string) x.pmtd_name (option i module_type) x.pmtd_type
    (attributes i) x.pmtd_attributes

and open_description i ppf x =
  fprintf ppf
    "@[<1>((popen_expr@ %a)@ (popen_override@ %a)@ (popen_attributes@ %a))@]"
    (pp_loc pp_longident) x.popen_expr override_flag x.popen_override
    (attributes i) x.popen_attributes

and open_declaration i ppf x =
  fprintf ppf
    "@[<1>((popen_expr@ %a)@ (popen_override@ %a)@ (popen_attributes@ %a))@]"
    (module_expr i) x.popen_expr override_flag x.popen_override (attributes i)
    x.popen_attributes

and include_description i ppf x =
  fprintf ppf "@[<1>((pincl_mod@ %a)@ (pincl_attributes@ %a))@]" (module_type i)
    x.pincl_mod (attributes i) x.pincl_attributes

and include_declaration i ppf x =
  fprintf ppf "@[<1>((pincl_mod@ %a)@ (pincl_attributes@ %a))@]" (module_expr i)
    x.pincl_mod (attributes i) x.pincl_attributes

and with_constraint i ppf = function
  | Pwith_type (li, decl) ->
    fprintf ppf "@[<1>(Pwith_type@ %a@ %a)@]" (pp_loc pp_longident) li
      (type_declaration i) decl
  | Pwith_module (li1, li2) ->
    fprintf ppf "@[<1>(Pwith_module@ %a@ %a)@]" (pp_loc pp_longident) li1
      (pp_loc pp_longident) li2
  | Pwith_modtype (li, t) ->
    fprintf ppf "@[<1>(Pwith_modtype@ %a@ %a)@]" (pp_loc pp_longident) li
      (module_type i) t
  | Pwith_modtypesubst (li, t) ->
    fprintf ppf "@[<1>(Pwith_modtypesubst@ %a@ %a)@]" (pp_loc pp_longident) li
      (module_type i) t
  | Pwith_typesubst (li, decl) ->
    fprintf ppf "@[<1>(Pwith_typesubst@ %a@ %a)@]" (pp_loc pp_longident) li
      (type_declaration i) decl
  | Pwith_modsubst (li1, li2) ->
    fprintf ppf "@[<1>(Pwith_modsubst@ %a@ %a)@]" (pp_loc pp_longident) li1
      (pp_loc pp_longident) li2

and module_expr i ppf x =
  fprintf ppf "@[<1>((pmod_desc@ %a)@ (pmod_attributes@ %a))@]"
    (module_expr_desc i) x.pmod_desc (attributes i) x.pmod_attributes

and module_expr_desc i ppf = function
  | Pmod_ident li ->
    fprintf ppf "@[<1>(Pmod_ident@ %a)@]" (pp_loc pp_longident) li
  | Pmod_structure str ->
    fprintf ppf "@[<1>(Pmod_structure@ %a)@]" (structure i) str
  | Pmod_functor (param, body) ->
    fprintf ppf "@[<1>(Pmod_functor@ %a@ %a)@]" (functor_parameter i) param
      (module_expr i) body
  | Pmod_apply (module1, module2) ->
    fprintf ppf "@[<1>(Pmod_apply@ %a@ %a)@]" (module_expr i) module1
      (module_expr i) module2
  | Pmod_apply_unit module_ ->
    fprintf ppf "@[<1>(Pmod_apply_unit@ %a)@]" (module_expr i) module_
  | Pmod_constraint (module_, type_) ->
    fprintf ppf "@[<1>(Pmod_constraint@ %a@ %a)@]" (module_expr i) module_
      (module_type i) type_
  | Pmod_unpack e -> fprintf ppf "@[<1>(Pmod_unpack@ %a)@]" (expression i) e
  | Pmod_extension ext ->
    fprintf ppf "@[<1>(Pmod_extension@ %a)@]" (extension i) ext

and structure i ppf x = list i structure_item ppf x

and structure_item i ppf x =
  fprintf ppf "@[<1>(pstr_desc@ %a)@]" (structure_item_desc i) x.pstr_desc

and structure_item_desc i ppf = function
  | Pstr_eval (e, attrs) ->
    fprintf ppf "@[<1>(Pstr_eval@ %a@ %a)@]" (expression i) e (attributes i)
      attrs
  | Pstr_value (rec_, bindings) ->
    fprintf ppf "@[<1>(Pstr_value@ %a@ %a)@]" rec_flag rec_
      (list i value_binding) bindings
  | Pstr_primitive value ->
    fprintf ppf "@[<1>(Pstr_primitive@ %a)@]" (value_description i) value
  | Pstr_type (rec_, types) ->
    fprintf ppf "@[<1>(Pstr_type@ %a@ %a)@]" rec_flag rec_
      (list i type_declaration) types
  | Pstr_typext ext ->
    fprintf ppf "@[<1>(Pstr_typext@ %a)@]" (type_extension i) ext
  | Pstr_exception exn ->
    fprintf ppf "@[<1>(Pstr_exception@ %a)@]" (type_exception i) exn
  | Pstr_module module_ ->
    fprintf ppf "@[<1>(Pstr_module@ %a)@]" (module_binding i) module_
  | Pstr_recmodule modules ->
    fprintf ppf "@[<1>(Pstr_recmodule@ %a)@]" (list i module_binding) modules
  | Pstr_modtype decl ->
    fprintf ppf "@[<1>(Pstr_modtype@ %a)@]" (module_type_declaration i) decl
  | Pstr_open open_ ->
    fprintf ppf "@[<1>(Pstr_open@ %a)@]" (open_declaration i) open_
  | Pstr_class classes ->
    fprintf ppf "@[<1>(Pstr_class@ %a)@]" (list i class_declaration) classes
  | Pstr_class_type classes ->
    fprintf ppf "@[<1>(Pstr_class_type@ %a)@]"
      (list i class_type_declaration)
      classes
  | Pstr_include include_ ->
    fprintf ppf "@[<1>(Pstr_include@ %a)@]" (include_declaration i) include_
  | Pstr_attribute attr ->
    fprintf ppf "@[<1>(Pstr_attribute@ %a)@]" (attribute i) attr
  | Pstr_extension (ext, attrs) ->
    fprintf ppf "@[<1>(Pstr_extension@ %a@ %a)@]" (extension i) ext
      (attributes i) attrs

and value_constraint i ppf = function
  | Pvc_constraint { locally_abstract_univars; typ } ->
    fprintf ppf
      "@[<1>(Pvc_constraint@ ((locally_abstract_univars@ %a)@ (typ@ %a)))@]"
      (pp_list (pp_loc pp_string))
      locally_abstract_univars (core_type i) typ
  | Pvc_coercion { ground; coercion } ->
    fprintf ppf "@[<1>(Pvc_coercion@ ((ground@ %a)@ (coercion@ %a)))@]"
      (option i core_type) ground (core_type i) coercion

and value_binding i ppf x =
  fprintf ppf
    "@[<1>((pvb_pat@ %a)@ (pvb_expr@ %a)@ (pvb_constraint@ %a)@ \
     (pvb_attributes@ %a))@]"
    (pattern i) x.pvb_pat (expression i) x.pvb_expr
    (option i value_constraint)
    x.pvb_constraint (attributes i) x.pvb_attributes

and module_binding i ppf x =
  fprintf ppf "@[<1>((pmb_name@ %a)@ (pmb_expr@ %a)@ (pmb_attributes@ %a))@]"
    (pp_loc pp_string_option) x.pmb_name (module_expr i) x.pmb_expr
    (attributes i) x.pmb_attributes

and attribute i ppf x =
  fprintf ppf "@[<1>((attr_name@ %a)@ (attr_payload@ %a))@]" (pp_loc pp_string)
    x.attr_name (payload i) x.attr_payload

and attributes i ppf xs = list i attribute ppf xs

and extension i ppf (name, payload_) =
  fprintf ppf "@[<1>(%a@ %a)@]" (pp_loc pp_string) name (payload i) payload_

and payload i ppf = function
  | PStr str -> fprintf ppf "@[<1>(PStr@ %a)@]" (structure i) str
  | PSig sg -> fprintf ppf "@[<1>(PSig@ %a)@]" (signature i) sg
  | PTyp type_ -> fprintf ppf "@[<1>(PTyp@ %a)@]" (core_type i) type_
  | PPat (pattern_, guard) ->
    fprintf ppf "@[<1>(PPat@ %a@ %a)@]" (pattern i) pattern_
      (option i expression) guard

and longident_loc _ ppf li = pp_loc pp_longident ppf li

and core_type_x_core_type i ppf (t1, t2, _loc) =
  fprintf ppf "@[<1>(%a@ %a)@]" (core_type i) t1 (core_type i) t2

and longident_x_core_type i ppf (li, t) =
  fprintf ppf "@[<1>(%a@ %a)@]" (pp_loc pp_longident) li (core_type i) t

and longident_x_pattern i ppf (li, p) =
  fprintf ppf "@[<1>(%a@ %a)@]" (pp_loc pp_longident) li (pattern i) p

and longident_x_expression i ppf (li, e) =
  fprintf ppf "@[<1>(%a@ %a)@]" (pp_loc pp_longident) li (expression i) e

and arg_label_x_expression i ppf (label, e) =
  fprintf ppf "@[<1>(%a@ %a)@]" arg_label label (expression i) e

and label_x_expression i ppf (label, e) =
  fprintf ppf "@[<1>(%a@ %a)@]" (pp_loc pp_string) label (expression i) e

let rec toplevel_phrase i ppf = function
  | Ptop_def str -> fprintf ppf "@[<1>(Ptop_def@ %a)@]" (structure i) str
  | Ptop_dir directive ->
    fprintf ppf "@[<1>(Ptop_dir@ %a)@]" (toplevel_directive i) directive

and toplevel_directive i ppf x =
  fprintf ppf "@[<1>((pdir_name@ %a)@ (pdir_arg@ %a))@]" (pp_loc pp_string)
    x.pdir_name
    (option i directive_argument)
    x.pdir_arg

and directive_argument i ppf x =
  fprintf ppf "@[<1>(pdira_desc@ %a)@]" (directive_argument_desc i) x.pdira_desc

and directive_argument_desc _ ppf = function
  | Pdir_string s -> fprintf ppf "@[<1>(Pdir_string@ %S)@]" s
  | Pdir_int (n, suffix) ->
    fprintf ppf "@[<1>(Pdir_int@ %S@ %a)@]" n pp_char_option suffix
  | Pdir_ident li -> fprintf ppf "@[<1>(Pdir_ident@ %a)@]" pp_longident li
  | Pdir_bool b -> fprintf ppf "@[<1>(Pdir_bool@ %a)@]" pp_bool b

let interface ppf x = signature 0 ppf x
let implementation ppf x = structure 0 ppf x
let top_phrase ppf x = toplevel_phrase 0 ppf x
