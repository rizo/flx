open struct
  let longident_unflatten = Longident.unflatten

  module Target_ast = Astlib.Ast_504
  module Parsetree = Target_ast.Parsetree
  module Asttypes = Target_ast.Asttypes
  module Longident = Target_ast.Longident
end

(* Assert current AST version: [Ast] must match [Ast_helper]. *)
(* let _ : char -> Target_ast.Parsetree.constant = Ast_helper.Const.char *)

open Prelude

module Ml = struct
  module Attr = Ast_helper.Attr
  module Const = Ast_helper.Const
  module Exp = Ast_helper.Exp
  module Pat = Ast_helper.Pat
  module Str = Ast_helper.Str
  module Sig = Ast_helper.Sig
  module Typ = Ast_helper.Typ
  module Type = Ast_helper.Type
  module Te = Ast_helper.Te
  module Vb = Ast_helper.Vb
  module Val = Ast_helper.Val
  module Cstr = Ast_helper.Cstr
  module Rf = Ast_helper.Rf
  module Mod = Ast_helper.Mod
  module Mty = Ast_helper.Mty
  module Mb = Ast_helper.Mb
  module Md = Ast_helper.Md
  module Mtd = Ast_helper.Mtd
  module Opn = Ast_helper.Opn
  module Incl = Ast_helper.Incl

  module Vc = struct
    let constraint_ vars typ =
      Parsetree.Pvc_constraint { locally_abstract_univars = vars; typ }

    let coercion ?ground coercion = Parsetree.Pvc_coercion { ground; coercion }
  end

  module Fun = struct
    let param ?(loc = Location.none) desc =
      { Parsetree.pparam_loc = loc; pparam_desc = desc }

    let val_param ?(label = Asttypes.Nolabel) ?default pat =
      param (Parsetree.Pparam_val (label, default, pat))

    let body exp = Parsetree.Pfunction_body exp
    let cases cases = Parsetree.Pfunction_cases (cases, Location.none, [])
  end

  module Case = struct
    let mk ?guard:pc_guard pc_lhs pc_rhs = { Parsetree.pc_lhs; pc_guard; pc_rhs }
  end

  let mknoloc = Location.mknoloc
  let mkloc loc x = Location.mkloc x loc
  let ident_noloc xs = mknoloc (Option.get (longident_unflatten xs))
  let ident ~loc xs = mkloc loc (Option.get (longident_unflatten xs))
end

(* FIXME: Delete this placeholder. *)
let loc = Location.none

let is_upper_name id =
  match id with
  | "" -> false
  | _ -> (
    match id.[0] with
    | 'A' .. 'Z' -> true
    | _ -> false
  )

module rec E_core_type : sig
  val eval : Flx.t -> Parsetree.core_type
end = struct
  let eval_path items =
    List.map
      (function
        | `id id -> id
        | _ -> failwith "unexpected dot segment"
        )
      items

  let eval_poly_var fl =
    match fl with
    | `id id -> Ml.mkloc loc (String.lowercase_ascii id)
    | _ -> fail "invalid type variable: %a" Flx.pp fl

  let eval_poly_vars fl =
    match fl with
    | `comma vars_fl -> List.map eval_poly_var vars_fl
    | var_fl -> [ eval_poly_var var_fl ]

  let rec eval (fl : Flx.t) =
    match fl with
    (* _ *)
    | `id "_" -> Ml.Typ.any ~loc ()
    (* None *)
    | `id id when is_upper_name id -> Ml.Typ.var ~loc (String.lowercase_ascii id)
    (* int *)
    | `id id -> Ml.Typ.constr ~loc (Ml.ident_noloc [ id ]) []
    | `dot path_id -> Ml.Typ.constr ~loc (Ml.ident ~loc (eval_path path_id)) []
    (* result[A, int] *)
    | `seq [ `id id; `brackets args_fl ] ->
      Ml.Typ.constr ~loc (Ml.ident_noloc [ id ]) (eval_args args_fl)
    (* Result.t[A, int] *)
    | `seq [ `dot path_id; `brackets args_fl ] ->
      Ml.Typ.constr ~loc (Ml.ident ~loc (eval_path path_id)) (eval_args args_fl)
    (* (int, option[bool]) *)
    | `parens (`comma items) -> Ml.Typ.tuple ~loc (List.map eval_tuple_item items)
    (* (A @ int) *)
    | `infix (_, "@", `id alias, typ_fl) ->
      Ml.Typ.alias ~loc (eval typ_fl) (Ml.mkloc loc (String.lowercase_ascii alias))
    | `parens typ -> eval typ
    (* (~l: int) -> bool *)
    | `infix
        ( _,
          "->",
          `parens (`seq [ `prefix (_, "~", `postfix (_, ":", `id l)); arg_fl ]),
          ret_fl
        ) -> Ml.Typ.arrow ~loc (Asttypes.Labelled l) (eval arg_fl) (eval ret_fl)
    (* (~l?: int) -> bool *)
    | `infix
        ( _,
          "->",
          `parens (`seq [ `prefix (_, "~", `postfix (_, "?:", `id l)); arg_fl ]),
          ret_fl
        ) -> Ml.Typ.arrow ~loc (Asttypes.Optional l) (eval arg_fl) (eval ret_fl)
    (* FIXME: review precedence *)
    (* [A] :: T1 -> T2 ("::" binds tighter than "->") *)
    | `infix (_, "->", `infix (_, "::", `brackets vars_fl, arg_fl), ret_fl) ->
      Ml.Typ.poly ~loc (eval_poly_vars vars_fl)
        (Ml.Typ.arrow ~loc Asttypes.Nolabel (eval arg_fl) (eval ret_fl))
    | `infix (_, "->", left, right) ->
      Ml.Typ.arrow ~loc Asttypes.Nolabel (eval left) (eval right)
    (* [A] :: T *)
    | `infix (_, "::", `brackets vars_fl, typ_fl) ->
      Ml.Typ.poly ~loc (eval_poly_vars vars_fl) (eval typ_fl)
    (* { #A | #B ... } *)
    | `braces (`pipe (first_row_fl :: rows_fl)) ->
      begin match first_row_fl with
      | `prefix (_, ">", row_fl) ->
        Ml.Typ.variant ~loc (List.map eval_row (row_fl :: rows_fl)) Asttypes.Open None
      | row_fl ->
        Ml.Typ.variant ~loc (List.map eval_row (row_fl :: rows_fl)) Asttypes.Closed None
      end
    (* {> #A } *)
    | `braces (`prefix (_, ">", row_fl)) ->
      Ml.Typ.variant ~loc [ eval_row row_fl ] Asttypes.Open None
    (* { #A } *)
    | `braces row_fl -> Ml.Typ.variant ~loc [ eval_row row_fl ] Asttypes.Closed None
    | _ -> wip "typ" fl

  and eval_args fl =
    match fl with
    | `comma args_fl -> List.map eval args_fl
    | arg_fl -> [ eval arg_fl ]

  and eval_tuple_item fl =
    match fl with
    (* ~l: T *)
    | `seq [ `prefix (_, "~", `postfix (_, ":", `id l)); typ_fl ] -> (Some l, eval typ_fl)
    | typ_fl -> (None, eval typ_fl)

  and eval_row fl =
    match fl with
    (* #A *)
    | `prefix (_, "#", `id tag) -> Ml.Rf.tag ~loc (Ml.mkloc loc tag) true []
    (* #A T1 T2 *)
    | `seq (`prefix (_, "#", `id tag) :: args_fl) ->
      Ml.Rf.tag ~loc (Ml.mkloc loc tag) false (List.map eval args_fl)
    | _ -> wip "row_field" fl
end

and E_value_constraint : sig
  val eval : Flx.t -> Parsetree.value_constraint
end = struct
  let eval_var fl =
    match fl with
    | `id id -> Ml.mkloc loc id
    | _ -> fail "invalid type variable: %a" Flx.pp fl

  let eval_vars fl =
    match fl with
    | `comma vars_fl -> List.map eval_var vars_fl
    | var_fl -> [ eval_var var_fl ]

  let eval (fl : Flx.t) =
    match fl with
    (* t1 :> t2 *)
    | `infix (_, ":>", ground_fl, coercion_fl) ->
      Ml.Vc.coercion ~ground:(E_core_type.eval ground_fl) (E_core_type.eval coercion_fl)
    (* type [a] :: a -> a ("::" binds tighter than "->") *)
    | `infix
        (_, "->", `infix (_, "::", `seq [ `id "type"; `brackets vars_fl ], arg_fl), ret_fl)
      ->
      let typ_ml =
        Ml.Typ.arrow ~loc Asttypes.Nolabel (E_core_type.eval arg_fl)
          (E_core_type.eval ret_fl)
      in
      Ml.Vc.constraint_ (eval_vars vars_fl) typ_ml
    (* type [a] :: a *)
    | `infix (_, "::", `seq [ `id "type"; `brackets vars_fl ], typ_fl) ->
      Ml.Vc.constraint_ (eval_vars vars_fl) (E_core_type.eval typ_fl)
    | typ_fl ->
      let typ_ml = E_core_type.eval typ_fl in
      Ml.Vc.constraint_ [] typ_ml
end

and Eval_case : sig
  val eval : Flx.t -> Parsetree.case
end = struct
  let eval (fl : Flx.t) =
    match fl with
    | `infix (_, "->", pat, exp) ->
      let pat_ml = E_pattern.eval pat in
      let exp_ml = E_expression.eval exp in
      Ml.Case.mk pat_ml exp_ml
    | _ -> wip "case" fl
end

and E_expression : sig
  val eval : Flx.t -> Parsetree.expression
  val eval_function_params : Flx.t -> Parsetree.function_param list
end = struct
  let eval_unit () = Ml.Exp.construct ~loc (Ml.ident_noloc [ "()" ]) None

  let eval_path items =
    List.map
      (function
        | `id id -> id
        | _ -> failwith "unexpected dot segment"
        )
      items

  let eval_function_params fl =
    match fl with
    (* ~l *)
    | `prefix (_, "~", `id label) ->
      let pat = Ml.Pat.var ~loc (Ml.mkloc loc label) in
      [ Ml.Fun.val_param ~label:(Asttypes.Labelled label) pat ]
    (* ~(l as p) *)
    | `prefix (_, "~", `parens (`seq [ `id label; `id "as"; pat_fl ])) ->
      [ Ml.Fun.val_param ~label:(Asttypes.Labelled label) (E_pattern.eval pat_fl) ]
    (* ~o? *)
    | `prefix (_, "~", `postfix (_, "?", `id label)) ->
      let pat = Ml.Pat.var ~loc (Ml.mkloc loc label) in
      [ Ml.Fun.val_param ~label:(Asttypes.Optional label) pat ]
    (* ~(o? as p) *)
    | `prefix (_, "~", `parens (`seq [ `postfix (_, "?", `id label); `id "as"; pat_fl ]))
      -> [ Ml.Fun.val_param ~label:(Asttypes.Optional label) (E_pattern.eval pat_fl) ]
    (* ~(o = default) *)
    | `prefix (_, "~", `parens (`infix (_, "=", `id label, default_fl))) ->
      let pat = Ml.Pat.var ~loc (Ml.mkloc loc label) in
      let default = E_expression.eval default_fl in
      [ Ml.Fun.val_param ~label:(Asttypes.Optional label) ~default pat ]
    (* ~(o as p = default) *)
    | `prefix
        ( _,
          "~",
          `parens (`infix (_, "=", `seq [ `id label; `id "as"; pat_fl ], default_fl))
        ) ->
      let default = E_expression.eval default_fl in
      [
        Ml.Fun.val_param ~label:(Asttypes.Optional label) ~default (E_pattern.eval pat_fl);
      ]
    (* (type t u) *)
    | `parens (`seq (`id "type" :: names_fl)) ->
      List.map
        (function
          | `id name -> Ml.Fun.param (Parsetree.Pparam_newtype (Ml.mkloc loc name))
          | name_fl -> wip "newtype param" name_fl
          )
        names_fl
    | _ ->
      let pat = E_pattern.eval fl in
      [ Ml.Fun.val_param pat ]

  let eval_apply_argument fl =
    match fl with
    (* ~a *)
    | `prefix (_, "~", `id lbl) ->
      (Asttypes.Labelled lbl, Ml.Exp.ident ~loc (Ml.ident ~loc [ lbl ]))
    (* ~b? *)
    | `prefix (_, "~", `postfix (_, "?", `id lbl)) ->
      (Asttypes.Optional lbl, Ml.Exp.ident ~loc (Ml.ident ~loc [ lbl ]))
    (* ~(a = e) *)
    | `prefix (_, "~", `parens (`infix (_, "=", `id lbl, e_fl))) ->
      (Asttypes.Labelled lbl, E_expression.eval e_fl)
    (* ~(b? = e) *)
    | `prefix (_, "~", `parens (`infix (_, "=", `postfix (_, "?", `id lbl), e_fl))) ->
      (Asttypes.Optional lbl, E_expression.eval e_fl)
    (* a *)
    | arg_fl -> (Asttypes.Nolabel, E_expression.eval arg_fl)

  let eval_tuple_item fl =
    match fl with
    (* ~l: e *)
    | `seq [ `prefix (_, "~", `postfix (_, ":", `id l)); e_fl ] ->
      (Some l, E_expression.eval e_fl)
    | e_fl -> (None, E_expression.eval e_fl)

  (* TODO: Handle field name casing. *)
  let eval_record_field fl =
    match fl with
    | `infix (_, "=", `id id_str, e_fl) ->
      let id_ml = Ml.ident ~loc [ id_str ] in
      let e_ml = E_expression.eval e_fl in
      (id_ml, e_ml)
    | _ -> fail "invalid record field: %a" Flx.pp fl

  (* TODO: Handle field name casing. *)
  let eval_class_field fl =
    match fl with
    (* TODO: inherit *)
    (* TODO: val *)
    | `infix (_, "=", `id id_str, e_fl) ->
      let id_ml = Ml.mkloc loc id_str in
      let e_ml = E_expression.eval e_fl in
      let field_kind = Parsetree.Cfk_concrete (Asttypes.Fresh, Ml.Exp.poly e_ml None) in
      Ast_helper.Cf.method_ id_ml Asttypes.Public field_kind
    (* m : T = e *)
    | `infix (_, "=", `infix (_, ":", `id id_str, typ_fl), e_fl) ->
      let id_ml = Ml.mkloc loc id_str in
      let e_ml = E_expression.eval e_fl in
      let typ_ml = E_core_type.eval typ_fl in
      let field_kind =
        Parsetree.Cfk_concrete (Asttypes.Fresh, Ml.Exp.poly e_ml (Some typ_ml))
      in
      Ast_helper.Cf.method_ id_ml Asttypes.Public field_kind
    (* TODO: private method *)
    (* TODO: vitrual method *)
    (* TODO: constraint *)
    (* TODO: initializer *)
    (* TODO: attribute *)
    (* TODO: extension *)
    | _ -> fail "invalid record field: %a" Flx.pp fl

  let eval_object_fields fields_fl =
    let self_pat_ml, fields_fl =
      match fields_fl with
      (* as self *)
      | `seq [ `id "as"; `id self ] :: rest -> (Ml.Pat.var ~loc (Ml.mkloc loc self), rest)
      (* as self @ p *)
      | `infix (_, "@", `seq [ `id "as"; `id self ], pat_fl) :: rest ->
        (Ml.Pat.alias (E_pattern.eval pat_fl) (Ml.mkloc loc self), rest)
      | fields_fl -> (Ml.Pat.any (), fields_fl)
    in
    Ml.Cstr.mk self_pat_ml (List.map eval_class_field fields_fl)

  let eval (fl : Flx.t) =
    match fl with
    (* [] *)
    | `brackets (`seq []) -> Ml.Exp.construct ~loc (Ml.ident ~loc [ "[]" ]) None
    (* true *)
    | `id "True" -> Ml.Exp.construct ~loc (Ml.ident ~loc [ "true" ]) None
    (* false *)
    | `id "False" -> Ml.Exp.construct ~loc (Ml.ident ~loc [ "false" ]) None
    (* 5 *)
    | `int x -> Ml.Exp.constant (Ml.Const.int x)
    (* "abc" *)
    | `str x -> Ml.Exp.constant (Ml.Const.string x)
    (* 'c' *)
    | `char x -> Ml.Exp.constant (Ml.Const.char x)
    (* 3l / 3L / 3n *)
    | `seq [ `int x; `id (("l" | "L" | "n") as suffix) ] ->
      Ml.Exp.constant (Ml.Const.int ~suffix:suffix.[0] x)
    (* --- Pexp_construct --- *)
    (* C *)
    | `id id when is_upper_name id -> Ml.Exp.construct ~loc (Ml.ident_noloc [ id ]) None
    (* C e / C e1 e2 ... *)
    | `seq (`id id :: args_fl) when is_upper_name id ->
      let arg_ml =
        match args_fl with
        | [ arg_fl ] -> E_expression.eval arg_fl
        | args_fl -> Ml.Exp.tuple ~loc (List.map eval_tuple_item args_fl)
      in
      Ml.Exp.construct ~loc (Ml.ident_noloc [ id ]) (Some arg_ml)
    (* foo *)
    | `id id -> Ml.Exp.ident ~loc (Ml.ident_noloc [ id ])
    (* --- Pexp_ident --- *)
    (* M.a *)
    | `dot (`id head :: _ as path_id) when is_upper_name head ->
      Ml.Exp.ident ~loc (Ml.ident ~loc (eval_path path_id))
    (* --- Pexp_field --- *)
    (* r.a / r.(X.a) *)
    | `dot (obj_fl :: fields_fl) ->
      List.fold_left
        (fun acc_ml field_fl ->
          match field_fl with
          | `id field -> Ml.Exp.field ~loc acc_ml (Ml.ident_noloc [ field ])
          | `parens (`dot path_fl) ->
            Ml.Exp.field ~loc acc_ml (Ml.ident ~loc (eval_path path_fl))
          | `parens (`id field) -> Ml.Exp.field ~loc acc_ml (Ml.ident_noloc [ field ])
          | _ -> fail "invalid field access: %a" Flx.pp field_fl
        )
        (E_expression.eval obj_fl) fields_fl
    | `parens (`seq []) -> eval_unit ()
    (* (items...,) _ *)
    | `parens (`comma items) -> Ml.Exp.tuple ~loc (List.map eval_tuple_item items)
    (* (_) *)
    | `parens exp -> E_expression.eval exp
    (* --- Pexp_let --- *)
    (* let a = 1, and b = 2, body / rec a = 1, body *)
    | `comma
        (`infix (_, "=", `seq [ `id (("let" | "rec") as kw); pat_fl ], exp_fl) :: rest_fl)
      ->
      let mk_vb pat_fl exp_fl =
        Ml.Vb.mk (E_pattern.eval pat_fl) (E_expression.eval exp_fl)
      in
      let rec collect acc rest_fl =
        match rest_fl with
        | [ body_fl ] -> (List.rev acc, E_expression.eval body_fl)
        | `infix (_, "=", `seq [ `id "and"; pat_fl ], exp_fl) :: rest_fl ->
          collect (mk_vb pat_fl exp_fl :: acc) rest_fl
        | _ -> fail "invalid let binding"
      in
      let vbs, body = collect [ mk_vb pat_fl exp_fl ] rest_fl in
      let rec_flag =
        if String.equal kw "rec" then Asttypes.Recursive else Asttypes.Nonrecursive
      in
      Ml.Exp.let_ rec_flag vbs body
    (* --- Pexp_lazy --- *)
    | `seq [ `id "lazy"; e1 ] ->
      let e1_ml = E_expression.eval e1 in
      Ml.Exp.lazy_ e1_ml
    (* --- Pexp_lazy --- *)
    | `seq (`id "lazy" :: _e1 :: _extra) -> fail "lazy requires a single expression"
    (* --- Pexp_match --- *)
    (* match e1 { cases..., } *)
    | `seq [ `id "match"; e1; `braces (`comma cases) ] ->
      let e1_ml = E_expression.eval e1 in
      Ml.Exp.match_ e1_ml (List.map Eval_case.eval cases)
    (* err: match e1 _ *)
    | `seq [ `id "match"; _e1; `braces (`seq []) ] -> fail "match with no cases"
    (* match e1 { cases..., } *)
    | `seq [ `id "match"; e1; `braces case ] ->
      let e1_ml = E_expression.eval e1 in
      Ml.Exp.match_ e1_ml [ Eval_case.eval case ]
    | `seq [ `id "match"; _e1; _ ] -> fail "missing braces around match cases"
    (* err: match e1 _ ... *)
    | `seq (`id "match" :: _e1 :: _cases :: _extra :: _rest) ->
      fail "invalid match syntax: did you forget a semicolon?"
    (* --- Pexp_try --- *)
    (* try e1 { cases..., } *)
    | `seq [ `id "try"; e1_fl; `braces (`comma cases_fl) ] ->
      let e1_ml = E_expression.eval e1_fl in
      Ml.Exp.try_ ~loc e1_ml (List.map Eval_case.eval cases_fl)
    | `seq [ `id "try"; e1_fl; `braces case_fl ] ->
      let e1_ml = E_expression.eval e1_fl in
      Ml.Exp.try_ ~loc e1_ml [ Eval_case.eval case_fl ]
    (* --- Pexp_function --- *)
    | `infix (_, "->", `seq (`id "fn" :: params_fl), body_fl) ->
      let params_ml = List.concat_map eval_function_params params_fl in
      let body_ml = E_expression.eval body_fl in
      Ml.Exp.function_ ~loc params_ml None (Ml.Fun.body body_ml)
    (* fn { cases..., } *)
    | `seq [ `id "fn"; `braces (`comma cases) ] ->
      Ml.Exp.function_ ~loc [] None (Ml.Fun.cases (List.map Eval_case.eval cases))
    (* | `infix (_, "->", `seq (`id "fn" :: args), body) -> *)
    (*   Eval_fun.exp (List.map (fun arg -> Positional arg) args) body *)

    (* --- Pexp_ifthenelse --- *)
    (* if e1 { e2 } else { e3 } *)
    | `seq [ `id "if"; e1_fl; `braces e2_fl; `id "else"; `braces e3_fl ] ->
      let e1_ml = E_expression.eval e1_fl in
      let e2_ml = E_expression.eval e2_fl in
      let e3_ml = E_expression.eval e3_fl in
      Ml.Exp.ifthenelse ~loc e1_ml e2_ml (Some e3_ml)
    | `seq (`id "if" :: _e1 :: _e2 :: `id "else" :: _e3 :: _extra :: _rest) ->
      fail "invalid if syntax: did you forget a semicolon?"
    (* --- Pexp_while --- *)
    (* while test { body } *)
    | `seq [ `id "while"; e1_fl; `braces e2_fl ] ->
      let e1_ml = E_expression.eval e1_fl in
      let e2_ml = E_expression.eval e2_fl in
      Ml.Exp.while_ ~loc e1_ml e2_ml
    (* --- Pexp_for --- *)
    (* for (pat = start to stop) { body } *)
    | `seq
        [
          `id "for";
          `parens (`infix (_, "=", pat_fl, `seq [ start_fl; `id "to"; stop_fl ]));
          body_fl;
        ] ->
      let pat_ml = E_pattern.eval pat_fl in
      let start_ml = E_expression.eval start_fl in
      let stop_ml = E_expression.eval stop_fl in
      let body_ml = E_expression.eval body_fl in
      Ml.Exp.for_ ~loc pat_ml start_ml stop_ml Asttypes.Upto body_ml
    (* for (pat = start downto stop) { body } *)
    | `seq
        [
          `id "for";
          `parens (`infix (_, "=", pat_fl, `seq [ start_fl; `id "downto"; stop_fl ]));
          `braces body_fl;
        ] ->
      let pat_ml = E_pattern.eval pat_fl in
      let start_ml = E_expression.eval start_fl in
      let stop_ml = E_expression.eval stop_fl in
      let body_ml = E_expression.eval body_fl in
      Ml.Exp.for_ ~loc pat_ml start_ml stop_ml Asttypes.Downto body_ml
    (* --- Pexp_coerce --- *)
    (* (e : t1 :> t2) *)
    | `infix (_, ":", e_fl, `infix (_, ":>", t1_fl, t2_fl)) ->
      Ml.Exp.coerce ~loc (E_expression.eval e_fl)
        (Some (E_core_type.eval t1_fl))
        (E_core_type.eval t2_fl)
    (* (e :> t) *)
    | `infix (_, ":>", e_fl, t_fl) ->
      Ml.Exp.coerce ~loc (E_expression.eval e_fl) None (E_core_type.eval t_fl)
    (* --- Pexp_constraint --- *)
    (* (e : t) *)
    | `infix (_, ":", e_fl, t_fl) ->
      Ml.Exp.constraint_ ~loc (E_expression.eval e_fl) (E_core_type.eval t_fl)
    (* --- Pexp_setfield --- *)
    (* r.a := e / r.(X.a) := e *)
    | `infix (_, ":=", `dot path_fl, value_fl) ->
      begin match List.rev path_fl with
      | field_fl :: obj_rev_fl ->
        let obj_fl =
          match List.rev obj_rev_fl with
          | [ single_fl ] -> single_fl
          | items_fl -> `dot items_fl
        in
        let field_lid =
          match field_fl with
          | `id field -> Ml.ident_noloc [ field ]
          | `parens (`dot path_fl) -> Ml.ident ~loc (eval_path path_fl)
          | _ -> fail "invalid field access: %a" Flx.pp field_fl
        in
        Ml.Exp.setfield ~loc (E_expression.eval obj_fl) field_lid
          (E_expression.eval value_fl)
      | [] -> fail "invalid field assignment"
      end
    (* --- Pexp_setinstvar --- *)
    (* x <- e *)
    | `infix (_, "<-", `id var, e_fl) ->
      Ml.Exp.setinstvar ~loc (Ml.mkloc loc var) (E_expression.eval e_fl)
    (* --- Pexp_new --- *)
    (* new c / new M.c *)
    | `seq [ `id "new"; `id cls ] -> Ml.Exp.new_ ~loc (Ml.ident_noloc [ cls ])
    | `seq [ `id "new"; `dot path_fl ] ->
      Ml.Exp.new_ ~loc (Ml.ident ~loc (eval_path path_fl))
    (* --- Pexp_send --- *)
    | `infix (_, "#", obj_fl, `id meth_name) ->
      let obj_ml = E_expression.eval obj_fl in
      Ml.Exp.send obj_ml (Ml.mkloc loc meth_name)
    (* --- Pexp_variant --- *)
    (* #A *)
    | `prefix (_, "#", `id id_str) -> Ml.Exp.variant ~loc id_str None
    (* #A 1 *)
    | `seq [ `prefix (_, "#", `id id_str); e1_fl ] ->
      let e1_ml = E_expression.eval e1_fl in
      Ml.Exp.variant ~loc id_str (Some e1_ml)
    (* --- Pexp_override --- *)
    (* #{ ..self, x = 1, ... } *)
    | `prefix (_, "#", `braces (`comma (`prefix (_, "..", `id "self") :: fields_fl))) ->
      let eval_override_field fl =
        match fl with
        | `infix (_, "=", `id id_str, e_fl) ->
          (Ml.mkloc loc id_str, E_expression.eval e_fl)
        | _ -> fail "invalid override field: %a" Flx.pp fl
      in
      Ml.Exp.override ~loc (List.map eval_override_field fields_fl)
    (* --- Pexp_object --- *)
    (* #{} *)
    | `prefix (_, "#", `braces (`seq [])) ->
      Ml.Exp.object_ ~loc (Ml.Cstr.mk (Ml.Pat.any ()) [])
    (* #{ a = 1, ... } *)
    | `prefix (_, "#", `braces (`comma fields_fl)) ->
      Ml.Exp.object_ ~loc (eval_object_fields fields_fl)
    (* #{ a = 1 } *)
    | `prefix (_, "#", `braces (`infix (_, "=", _, _) as field_fl)) ->
      Ml.Exp.object_ ~loc (eval_object_fields [ field_fl ])
    (* --- Pexp_extension --- *)
    (* %ext *)
    | `prefix (_, "%", `id name) ->
      Ml.Exp.extension ~loc (Ml.mkloc loc name, Parsetree.PStr [])
    (* --- Pexp_assert --- *)
    (* assert exp *)
    | `seq [ `id "assert"; exp_fl ] ->
      let exp_ml = E_expression.eval exp_fl in
      Ml.Exp.assert_ ~loc exp_ml
    | `seq (f_fl :: args_fl) ->
      let f_ml = E_expression.eval f_fl in
      let args_ml = List.map eval_apply_argument args_fl in
      Ml.Exp.apply ~loc f_ml args_ml
    | `infix (_, op, e1_fl, e2_fl) ->
      let f_ml = Ml.Exp.ident ~loc (Ml.ident_noloc [ op ]) in
      let e1_ml = E_expression.eval e1_fl in
      let e2_ml = E_expression.eval e2_fl in
      Ml.Exp.apply ~loc f_ml [ (Asttypes.Nolabel, e1_ml); (Asttypes.Nolabel, e2_ml) ]
    (* -a *)
    | `prefix (_, op, e1_fl) ->
      let f_ml =
        let op = if String.equal op "-" then "~-" else op in
        let op = if String.equal op "+" then "~+" else op in
        Ml.Exp.ident ~loc (Ml.ident_noloc [ op ])
      in
      let e1_ml = E_expression.eval e1_fl in
      Ml.Exp.apply ~loc f_ml [ (Asttypes.Nolabel, e1_ml) ]
      (* --- Pexp_sequence --- *)
      (* {e1; e2; ...} *)
    | `braces (`semi items_fl) ->
      let items_ml = List.map E_expression.eval items_fl in
      Ml.Exp.array items_ml
    (* --- Pexp_record --- *)
    (* { ..r, x = 1, ... } *)
    | `braces (`comma (`prefix (_, "..", record_fl) :: (_ as record_fields_fl))) ->
      let record_ml = E_expression.eval record_fl in
      let fields_ml = List.map eval_record_field record_fields_fl in
      Ml.Exp.record fields_ml (Some record_ml)
    (* { x = 1, ... } *)
    | `braces (`comma (`infix (_, "=", _, _) :: _ as record_fields_fl)) ->
      let fields_ml = List.map eval_record_field record_fields_fl in
      Ml.Exp.record fields_ml None
    (* { x = 1 } *)
    | `braces (`infix (_, "=", _, _) as single_field_fl) ->
      let single_field_ml = eval_record_field single_field_fl in
      Ml.Exp.record [ single_field_ml ] None
    (* --- Pexp_array --- *)
    (* {} *)
    | `braces (`seq []) -> Ml.Exp.array []
    (* {a, b, ...} *)
    | `braces (`comma items_fl) ->
      let items_ml = List.map E_expression.eval items_fl in
      Ml.Exp.array items_ml
    (* {a} *)
    | `braces item_fl ->
      let item_ml = E_expression.eval item_fl in
      Ml.Exp.array [ item_ml ]
    (* --- Pexp_unreachable --- *)
    | `op "!" -> Ml.Exp.unreachable ~loc ()
    | _ -> wip "exp" fl
end

and E_pattern : sig
  val eval : Flx.t -> Parsetree.pattern
end = struct
  let rec eval (fl : Flx.t) =
    match fl with
    (* 5 *)
    | `int x -> Ml.Pat.constant (Ml.Const.int x)
    (* "abc" *)
    | `str x -> Ml.Pat.constant (Ml.Const.string x)
    (* 'x' *)
    | `char x -> Ml.Pat.constant (Ml.Const.char x)
    (* 'a'..'z' *)
    | `infix (_, "..", `char a, `char b) ->
      Ml.Pat.interval ~loc (Ml.Const.char a) (Ml.Const.char b)
    (* () *)
    | `parens (`seq []) -> Ml.Pat.construct ~loc (Ml.ident_noloc [ "()" ]) None
    (* #A *)
    | `prefix (_, "#", `id tag) -> Ml.Pat.variant ~loc tag None
    (* #A p *)
    | `seq [ `prefix (_, "#", `id tag); arg_fl ] ->
      Ml.Pat.variant ~loc tag (Some (eval arg_fl))
    (* &t *)
    | `prefix (_, "&", `id t) -> Ml.Pat.type_ ~loc (Ml.ident_noloc [ t ])
    (* lazy p *)
    | `seq [ `id "lazy"; pat_fl ] -> Ml.Pat.lazy_ ~loc (eval pat_fl)
    (* mod _ / mod M *)
    | `seq [ `id "mod"; `id "_" ] -> Ml.Pat.unpack ~loc (Ml.mknoloc None)
    | `seq [ `id "mod"; `id name ] when is_upper_name name ->
      Ml.Pat.unpack ~loc (Ml.mknoloc (Some name))
    (* exn p *)
    | `seq [ `id "exn"; pat_fl ] -> Ml.Pat.exception_ ~loc (eval pat_fl)
    (* C *)
    | `id id when is_upper_name id -> Ml.Pat.construct ~loc (Ml.ident_noloc [ id ]) None
    (* C arg *)
    | `seq [ `id id; arg_fl ] when is_upper_name id ->
      Ml.Pat.construct ~loc (Ml.ident_noloc [ id ]) (Some ([], eval arg_fl))
    (* C args... *)
    | `seq (`id id :: args_fl) when is_upper_name id ->
      let types_list = [] in
      (* TODO: Closed? *)
      let args_ml =
        Ml.Pat.tuple ~loc (List.map (fun arg -> (None, eval arg)) args_fl) Closed
      in
      Ml.Pat.construct ~loc (Ml.ident_noloc [ id ]) (Some (types_list, args_ml))
    (* _ *)
    | `id "_" -> Ml.Pat.any ~loc ()
    (* a *)
    | `id id -> Ml.Pat.var ~loc (Ml.mknoloc id)
    (* (1, 'x', a) *)
    (* TODO: Closed? *)
    | `parens (`comma items) -> Ml.Pat.tuple ~loc (List.map eval_tuple_item items) Closed
    (* (_) *)
    | `parens pat -> eval pat
    (* (p : T) *)
    | `infix (_, ":", pat_fl, typ_fl) ->
      Ml.Pat.constraint_ ~loc (eval pat_fl) (E_core_type.eval typ_fl)
    (* _ | _ *)
    | `pipe items ->
      List.fold_left
        (fun acc pat -> Ml.Pat.or_ ~loc acc pat)
        (eval (List.hd items))
        (List.map eval (List.tl items))
    (* x @ _ *)
    | `infix (_, "@", `id alias, pat_fl) ->
      let pat_ml = eval pat_fl in
      Ml.Pat.alias pat_ml (Ml.mkloc loc alias)
    (* err: x @ _ *)
    | `infix (_, "@", _, _) -> fail "invalid pattern alias: alias must be an identifier"
    (* M.(p) *)
    | `dot [ `id m; `parens pat_fl ] when is_upper_name m ->
      Ml.Pat.open_ ~loc (Ml.ident_noloc [ m ]) (eval pat_fl)
    (* {} *)
    | `braces (`seq []) -> Ml.Pat.array ~loc []
    (* { ~a, b = p, _ } / { p1, p2, ... } *)
    | `braces (`comma items_fl) ->
      let is_record_field = function
        | `prefix (_, "~", `id _) | `infix (_, "=", `id _, _) -> true
        | _ -> false
      in
      if List.exists is_record_field items_fl then
        let closed =
          if
            List.exists
              (function
                | `id "_" -> true
                | _ -> false
                )
              items_fl
          then Asttypes.Open
          else Asttypes.Closed
        in
        let fields =
          List.filter_map
            (function
              | `id "_" -> None
              | `prefix (_, "~", `id name) ->
                Some (Ml.ident_noloc [ name ], Ml.Pat.var ~loc (Ml.mkloc loc name))
              | `infix (_, "=", `id name, pat_fl) ->
                Some (Ml.ident_noloc [ name ], eval pat_fl)
              | field_fl -> fail "invalid record field pattern: %a" Flx.pp field_fl
              )
            items_fl
        in
        Ml.Pat.record ~loc fields closed
      else Ml.Pat.array ~loc (List.map eval items_fl)
    | `seq [ item ] -> eval item
    | _ -> wip "pat" fl

  and eval_tuple_item fl =
    match fl with
    (* ~l *)
    | `prefix (_, "~", `id l) -> (Some l, Ml.Pat.var ~loc (Ml.mkloc loc l))
    | pat_fl -> (None, eval pat_fl)
end

and E_value_binding : sig
  val eval : Flx.t -> Parsetree.value_binding
end = struct
  let eval (fl : Flx.t) =
    match fl with
    (* a : int = 3 *)
    | `infix (_, "=", `infix (_, ":", pat_fl, vc_fl), exp_fl) ->
      let pat_ml = E_pattern.eval pat_fl in
      let vc_ml = E_value_constraint.eval vc_fl in
      let exp_ml = E_expression.eval exp_fl in
      Ml.Vb.mk ~value_constraint:vc_ml pat_ml exp_ml
    (* a = 3 *)
    | `infix (_, "=", pat_fl, exp_fl) ->
      let pat_ml = E_pattern.eval pat_fl in
      let exp_ml = E_expression.eval exp_fl in
      Ml.Vb.mk pat_ml exp_ml
    | _ -> wip "vb" fl
end

and E_constructor_declaration : sig
  val eval : Flx.t -> Parsetree.constructor_declaration
end = struct
  let eval fl =
    match fl with
    (* C *)
    | `id name when is_upper_name name -> Ml.Type.constructor ~loc (Ml.mkloc loc name)
    (* C { a : T, ... } *)
    | `seq [ `id name; `braces content_fl ] when is_upper_name name ->
      begin match E_type_declaration.eval_label_declarations content_fl with
      | Some fields ->
        Ml.Type.constructor ~loc ~args:(Parsetree.Pcstr_record fields) (Ml.mkloc loc name)
      | None ->
        Ml.Type.constructor ~loc
          ~args:(Parsetree.Pcstr_tuple [ E_core_type.eval (`braces content_fl) ])
          (Ml.mkloc loc name)
      end
    (* C ... *)
    | `seq (`id name :: args_fl) when is_upper_name name ->
      let args_ml = List.map E_core_type.eval args_fl in
      Ml.Type.constructor ~loc ~args:(Parsetree.Pcstr_tuple args_ml) (Ml.mkloc loc name)
    | _ -> wip "constructor_declaration" fl
end

and E_type_declaration : sig
  val eval : Flx.t -> (Asttypes.rec_flag * Parsetree.type_declaration list) option
  val eval_extension : Flx.t -> Parsetree.type_extension option
  val eval_exception : Flx.t -> Parsetree.type_exception option
  val eval_label_declarations : Flx.t -> Parsetree.label_declaration list option
end = struct
  let eval_type_param fl =
    match fl with
    | `id "_" ->
      let t = Ml.Typ.any ~loc () in
      (t, (Asttypes.NoVariance, Asttypes.NoInjectivity))
    | `id param_id ->
      let t = Ml.Typ.var ~loc (String.lowercase_ascii param_id) in
      (t, (Asttypes.NoVariance, Asttypes.NoInjectivity))
    | fl -> wip "type_param" fl

  let eval_params fl =
    match fl with
    | `comma params_fl -> List.map eval_type_param params_fl
    | param_fl -> [ eval_type_param param_fl ]

  (* a : T / a : mutable T *)
  let eval_label_declaration fl =
    match fl with
    | `infix (_, ":", `id name, `seq [ `id "mutable"; typ_fl ])
      when not (is_upper_name name) ->
      Some
        (Ml.Type.field ~mut:Asttypes.Mutable (Ml.mkloc loc name) (E_core_type.eval typ_fl))
    | `infix (_, ":", `id name, typ_fl) when not (is_upper_name name) ->
      Some (Ml.Type.field (Ml.mkloc loc name) (E_core_type.eval typ_fl))
    | _ -> None

  let eval_label_declarations fl =
    let items_fl =
      match fl with
      | `comma items_fl -> items_fl
      | item_fl -> [ item_fl ]
    in
    let fields = List.map eval_label_declaration items_fl in
    if List.for_all Option.is_some fields then Some (List.map Option.get fields) else None

  let eval_kind_and_manifest body_fl =
    match body_fl with
    (* {..} *)
    | `braces (`op "..") -> (Parsetree.Ptype_open, None)
    (* { a : T, ... } / { C | ... } *)
    | `braces content_fl ->
      begin match eval_label_declarations content_fl with
      | Some fields -> (Parsetree.Ptype_record fields, None)
      | None ->
        let constructors_fl =
          match content_fl with
          | `pipe constructors_fl -> constructors_fl
          | constructor_fl -> [ constructor_fl ]
        in
        ( Parsetree.Ptype_variant (List.map E_constructor_declaration.eval constructors_fl),
          None
        )
      end
    (* T *)
    | typ_fl -> (Parsetree.Ptype_abstract, Some (E_core_type.eval typ_fl))

  (* [nonrec] name [params...] *)
  let eval_head parts_fl =
    let nonrec_flag, parts_fl =
      match parts_fl with
      | `id "nonrec" :: parts_fl -> (true, parts_fl)
      | parts_fl -> (false, parts_fl)
    in
    match parts_fl with
    | [ `id name ] -> Some (nonrec_flag, name, [])
    | [ `id name; `brackets params_fl ] -> Some (nonrec_flag, name, eval_params params_fl)
    | _ -> None

  let eval_decl ?body_fl name params =
    match body_fl with
    | None -> Ml.Type.mk ~params (Ml.mkloc loc name)
    | Some body_fl ->
      let kind, manifest = eval_kind_and_manifest body_fl in
      Ml.Type.mk ~params ~kind ?manifest (Ml.mkloc loc name)

  (* and t = _ *)
  let eval_and_decl fl =
    match fl with
    | `infix (_, "=", `seq (`id "and" :: parts_fl), body_fl) ->
      begin match eval_head parts_fl with
      | Some (_, name, params) -> eval_decl ~body_fl name params
      | None -> fail "invalid type declaration: %a" Flx.pp fl
      end
    | _ -> fail "invalid type declaration: %a" Flx.pp fl

  let eval fl =
    match fl with
    (* type t / type t[A] *)
    | `seq (`id "type" :: parts_fl) ->
      begin match eval_head parts_fl with
      | Some (_, name, params) -> Some (Asttypes.Nonrecursive, [ eval_decl name params ])
      | None -> None
      end
    (* type t = _ *)
    | `infix (_, "=", `seq (`id "type" :: parts_fl), body_fl) ->
      begin match eval_head parts_fl with
      | Some (nonrec_flag, name, params) ->
        let rec_flag =
          if nonrec_flag then Asttypes.Nonrecursive else Asttypes.Recursive
        in
        Some (rec_flag, [ eval_decl ~body_fl name params ])
      | None -> None
      end
    (* type t1 = _, and t2 = _ *)
    | `comma (`infix (_, "=", `seq (`id "type" :: parts_fl), body_fl) :: and_fl) ->
      begin match eval_head parts_fl with
      | Some (_, name, params) ->
        Some
          ( Asttypes.Recursive,
            eval_decl ~body_fl name params :: List.map eval_and_decl and_fl
          )
      | None -> None
      end
    | _ -> None

  (* C / C T1 T2 / C { a : T } *)
  let eval_ext_constructor fl =
    match fl with
    | `id name when is_upper_name name -> Ml.Te.decl ~loc (Ml.mkloc loc name)
    | `seq [ `id name; `braces content_fl ] when is_upper_name name ->
      begin match eval_label_declarations content_fl with
      | Some fields ->
        Ml.Te.decl ~loc ~args:(Parsetree.Pcstr_record fields) (Ml.mkloc loc name)
      | None -> fail "invalid extension constructor: %a" Flx.pp fl
      end
    | `seq (`id name :: args_fl) when is_upper_name name ->
      Ml.Te.decl ~loc
        ~args:(Parsetree.Pcstr_tuple (List.map E_core_type.eval args_fl))
        (Ml.mkloc loc name)
    | _ -> fail "invalid extension constructor: %a" Flx.pp fl

  let eval_extension fl =
    match fl with
    (* type t += C ... *)
    | `infix (_, "+=", `seq (`id "type" :: parts_fl), constructor_fl) ->
      begin match eval_head parts_fl with
      | Some (_, name, params) ->
        Some
          (Ml.Te.mk ~params (Ml.ident_noloc [ name ])
             [ eval_ext_constructor constructor_fl ]
          )
      | None -> None
      end
    (* type t += C1 = C2 *)
    | `infix
        (_, "=", `infix (_, "+=", `seq (`id "type" :: parts_fl), `id name), `id target)
      when is_upper_name name && is_upper_name target ->
      begin match eval_head parts_fl with
      | Some (_, type_name, params) ->
        Some
          (Ml.Te.mk ~params
             (Ml.ident_noloc [ type_name ])
             [ Ml.Te.rebind ~loc (Ml.mkloc loc name) (Ml.ident_noloc [ target ]) ]
          )
      | None -> None
      end
    | _ -> None

  let eval_exception fl =
    match fl with
    (* exn E = F *)
    | `infix (_, "=", `seq [ `id "exn"; `id name ], `id target)
      when is_upper_name name && is_upper_name target ->
      Some
        (Ml.Te.mk_exception
           (Ml.Te.rebind ~loc (Ml.mkloc loc name) (Ml.ident_noloc [ target ]))
        )
    (* exn E / exn E T1 T2 *)
    | `seq (`id "exn" :: `id name :: args_fl) when is_upper_name name ->
      let args =
        match args_fl with
        | [] -> None
        | args_fl -> Some (Parsetree.Pcstr_tuple (List.map E_core_type.eval args_fl))
      in
      Some (Ml.Te.mk_exception (Ml.Te.decl ~loc ?args (Ml.mkloc loc name)))
    | _ -> None
end

and E_attribute : sig
  val eval : string -> Flx.t option -> Parsetree.attribute
end = struct
  let eval name payload_fl =
    let payload =
      match payload_fl with
      | None -> Parsetree.PStr []
      | Some e_fl -> Parsetree.PStr [ Ml.Str.eval ~loc (E_expression.eval e_fl) ]
    in
    Ml.Attr.mk (Ml.mkloc loc name) payload
end

and E_module_expr : sig
  val eval : Flx.t -> Parsetree.module_expr
end = struct
  let rec eval (fl : Flx.t) =
    match fl with
    (* X *)
    | `id name when is_upper_name name -> Ml.Mod.ident ~loc (Ml.ident_noloc [ name ])
    (* X.Y *)
    | `dot path_fl ->
      let path =
        List.map
          (function
            | `id id -> id
            | _ -> failwith "unexpected dot segment"
            )
          path_fl
      in
      Ml.Mod.ident ~loc (Ml.ident ~loc path)
    (* {} *)
    | `braces (`seq []) -> Ml.Mod.structure ~loc []
    (* { items... } *)
    | `braces (`semi items_fl) ->
      Ml.Mod.structure ~loc (List.map E_structure_item.eval items_fl)
    (* val e *)
    | `seq [ `id "val"; e_fl ] -> Ml.Mod.unpack ~loc (E_expression.eval e_fl)
    (* F X / F () *)
    | `seq (head_fl :: args_fl) ->
      List.fold_left
        (fun acc_ml arg_fl ->
          match arg_fl with
          | `parens (`seq []) -> Ml.Mod.apply_unit ~loc acc_ml
          | arg_fl -> Ml.Mod.apply ~loc acc_ml (eval arg_fl)
        )
        (eval head_fl) args_fl
    (* %ext *)
    | `prefix (_, "%", `id name) ->
      Ml.Mod.extension ~loc (Ml.mkloc loc name, Parsetree.PStr [])
    (* { item } *)
    | `braces item_fl -> Ml.Mod.structure ~loc [ E_structure_item.eval item_fl ]
    | _ -> wip "module_expr" fl
end

and E_module_type : sig
  val eval : Flx.t -> Parsetree.module_type
end = struct
  let eval (fl : Flx.t) =
    match fl with
    (* S *)
    | `id name when is_upper_name name -> Ml.Mty.ident ~loc (Ml.ident_noloc [ name ])
    (* M.S *)
    | `dot path_fl ->
      let path =
        List.map
          (function
            | `id id -> id
            | _ -> failwith "unexpected dot segment"
            )
          path_fl
      in
      Ml.Mty.ident ~loc (Ml.ident ~loc path)
    (* {} *)
    | `braces (`seq []) -> Ml.Mty.signature ~loc []
    (* { items... } *)
    | `braces (`semi items_fl) ->
      Ml.Mty.signature ~loc (List.map E_signature_item.eval items_fl)
    (* sig of ME *)
    | `seq [ `id "sig"; `id "of"; me_fl ] -> Ml.Mty.typeof_ ~loc (E_module_expr.eval me_fl)
    (* %ext *)
    | `prefix (_, "%", `id name) ->
      Ml.Mty.extension ~loc (Ml.mkloc loc name, Parsetree.PStr [])
    (* { item } *)
    | `braces item_fl -> Ml.Mty.signature ~loc [ E_signature_item.eval item_fl ]
    | _ -> wip "module_type" fl
end

and E_signature_item : sig
  val eval : Flx.t -> Parsetree.signature_item
end = struct
  let eval_open_path fl =
    match fl with
    | `id name -> Ml.ident_noloc [ name ]
    | `dot path_fl ->
      let path =
        List.map
          (function
            | `id id -> id
            | _ -> failwith "unexpected dot segment"
            )
          path_fl
      in
      Ml.ident ~loc path
    | _ -> fail "invalid open path: %a" Flx.pp fl

  let eval (fl : Flx.t) : Parsetree.signature_item =
    match E_type_declaration.eval fl with
    | Some (rec_flag, decls) -> Ml.Sig.type_ rec_flag decls
    | None -> (
      match E_type_declaration.eval_extension fl with
      | Some te -> Ml.Sig.type_extension te
      | None -> (
        match E_type_declaration.eval_exception fl with
        | Some exn -> Ml.Sig.exception_ exn
        | None -> (
          match fl with
          (* --- Psig_value --- *)
          (* val x : T *)
          | `infix (_, ":", `seq [ `id "val"; `id name ], typ_fl) ->
            Ml.Sig.value (Ml.Val.mk (Ml.mkloc loc name) (E_core_type.eval typ_fl))
          (* external f : T = "prim" *)
          | `infix
              ( _,
                "=",
                `infix (_, ":", `seq [ `id "external"; `id name ], typ_fl),
                `str prim
              ) ->
            Ml.Sig.value
              (Ml.Val.mk ~prim:[ prim ] (Ml.mkloc loc name) (E_core_type.eval typ_fl))
          (* --- Psig_module --- *)
          (* mod X : MT *)
          | `infix (_, ":", `seq [ `id "mod"; `id name ], mt_fl) ->
            Ml.Sig.module_ (Ml.Md.mk (Ml.mknoloc (Some name)) (E_module_type.eval mt_fl))
          (* mod X = M *)
          | `infix (_, "=", `seq [ `id "mod"; `id name ], `id target)
            when is_upper_name target ->
            Ml.Sig.module_
              (Ml.Md.mk (Ml.mknoloc (Some name))
                 (Ml.Mty.alias ~loc (Ml.ident_noloc [ target ]))
              )
          (* --- Psig_modtype --- *)
          (* sig T *)
          | `seq [ `id "sig"; `id name ] -> Ml.Sig.modtype (Ml.Mtd.mk (Ml.mkloc loc name))
          (* sig U = MT *)
          | `infix (_, "=", `seq [ `id "sig"; `id name ], mt_fl) ->
            Ml.Sig.modtype (Ml.Mtd.mk ~typ:(E_module_type.eval mt_fl) (Ml.mkloc loc name))
          (* --- Psig_open --- *)
          | `seq [ `id "open"; path_fl ] ->
            Ml.Sig.open_ (Ml.Opn.mk (eval_open_path path_fl))
          | `seq [ `postfix (_, "!", `id "open"); path_fl ] ->
            Ml.Sig.open_ (Ml.Opn.mk ~override:Asttypes.Override (eval_open_path path_fl))
          (* --- Psig_include --- *)
          | `seq (`id "include" :: mt_fl) ->
            let mt_fl =
              match mt_fl with
              | [ mt_fl ] -> mt_fl
              | mt_fl -> `seq mt_fl
            in
            Ml.Sig.include_ (Ml.Incl.mk (E_module_type.eval mt_fl))
          (* --- Psig_attribute --- *)
          | `brackets (`at (`id name, payload_fl)) ->
            Ml.Sig.attribute (E_attribute.eval name payload_fl)
          (* --- Psig_extension --- *)
          | `prefix (_, "%", `id name) ->
            Ml.Sig.extension (Ml.mkloc loc name, Parsetree.PStr [])
          | _ -> wip "signature_item" fl
        )
      )
    )
end

and E_structure_item : sig
  val eval : Flx.t -> Parsetree.structure_item
end = struct
  let eval_prim fl =
    match fl with
    | `str prim -> [ prim ]
    | `seq prims_fl ->
      List.map
        (function
          | `str prim -> prim
          | prim_fl -> fail "invalid primitive name: %a" Flx.pp prim_fl
          )
        prims_fl
    | _ -> fail "invalid primitive name: %a" Flx.pp fl

  (* val a = e / val f args... = e *)
  let eval_value_binding parts_fl exp_fl =
    match parts_fl with
    | [ vb_pat_fl ] -> Ml.Vb.mk (E_pattern.eval vb_pat_fl) (E_expression.eval exp_fl)
    | `id name :: args_fl ->
      let params_ml = List.concat_map E_expression.eval_function_params args_fl in
      let body_ml = Ml.Fun.body (E_expression.eval exp_fl) in
      let vb_exp = Ml.Exp.function_ ~loc params_ml None body_ml in
      Ml.Vb.mk (Ml.Pat.var ~loc (Ml.mkloc loc name)) vb_exp
    | _ -> fail "invalid value binding"

  let rec_flag_of_keyword kw =
    if String.equal kw "rec" then Asttypes.Recursive else Asttypes.Nonrecursive

  let eval (fl : Flx.t) : Parsetree.structure_item =
    match E_type_declaration.eval fl with
    | Some (rec_flag, decls) -> Ml.Str.type_ rec_flag decls
    | None -> (
      match E_type_declaration.eval_extension fl with
      | Some te -> Ml.Str.type_extension te
      | None -> (
        match E_type_declaration.eval_exception fl with
        | Some exn -> Ml.Str.exception_ exn
        | None -> (
          match fl with
          (* --- Pstr_value --- *)
          (* val a = 1 / val f args... = body / rec ... *)
          | `infix (_, "=", `seq (`id (("val" | "rec") as kw) :: parts_fl), exp_fl) ->
            Ml.Str.value ~loc (rec_flag_of_keyword kw)
              [ eval_value_binding parts_fl exp_fl ]
          (* val a : int = 1 *)
          | `infix
              ( _,
                "=",
                `infix (_, ":", `seq [ `id (("val" | "rec") as kw); pat_fl ], vc_fl),
                exp_fl
              ) ->
            let pat_ml = E_pattern.eval pat_fl in
            let vc_ml = E_value_constraint.eval vc_fl in
            let exp_ml = E_expression.eval exp_fl in
            let vb_ml = Ml.Vb.mk ~value_constraint:vc_ml pat_ml exp_ml in
            Ml.Str.value ~loc (rec_flag_of_keyword kw) [ vb_ml ]
          (* val a :> int = 1 *)
          | `infix
              (_, "=", `infix (_, ":>", `seq [ `id "val"; pat_fl ], coercion_fl), exp_fl)
            ->
            let vc_ml = Ml.Vc.coercion (E_core_type.eval coercion_fl) in
            let vb_ml =
              Ml.Vb.mk ~value_constraint:vc_ml (E_pattern.eval pat_fl)
                (E_expression.eval exp_fl)
            in
            Ml.Str.value ~loc Asttypes.Nonrecursive [ vb_ml ]
          (* val { a = 1, b = 2 } *)
          | `seq [ `id "val"; `braces (`comma vbl_fl) ] ->
            let vbl_ml = List.map E_value_binding.eval vbl_fl in
            Ml.Str.value ~loc Asttypes.Nonrecursive vbl_ml
          (* val a = 1, and b = 2 *)
          | `comma
              (`infix (_, "=", `seq (`id (("val" | "rec") as kw) :: parts_fl), exp_fl)
              :: and_fl
              )
            when List.for_all
                   (function
                     | `infix (_, "=", `seq (`id "and" :: _), _) -> true
                     | _ -> false
                     )
                   and_fl ->
            let eval_and fl =
              match fl with
              | `infix (_, "=", `seq (`id "and" :: parts_fl), exp_fl) ->
                eval_value_binding parts_fl exp_fl
              | _ -> fail "invalid value binding: %a" Flx.pp fl
            in
            Ml.Str.value ~loc (rec_flag_of_keyword kw)
              (eval_value_binding parts_fl exp_fl :: List.map eval_and and_fl)
          (* --- Pstr_primitive --- *)
          (* external x : T = "prim" *)
          | `infix
              (_, "=", `infix (_, ":", `seq [ `id "external"; `id name ], typ_fl), prim_fl)
            ->
            Ml.Str.primitive ~loc
              (Ml.Val.mk ~prim:(eval_prim prim_fl) (Ml.mkloc loc name)
                 (E_core_type.eval typ_fl)
              )
          (* --- Pstr_module --- *)
          (* mod X = ME *)
          | `infix (_, "=", `seq [ `id "mod"; `id name ], me_fl) ->
            Ml.Str.module_ ~loc
              (Ml.Mb.mk (Ml.mknoloc (Some name)) (E_module_expr.eval me_fl))
          (* mod X : MT = ME *)
          | `infix (_, "=", `infix (_, ":", `seq [ `id "mod"; `id name ], mt_fl), me_fl)
            ->
            Ml.Str.module_ ~loc
              (Ml.Mb.mk (Ml.mknoloc (Some name))
                 (Ml.Mod.constraint_ ~loc (E_module_expr.eval me_fl)
                    (E_module_type.eval mt_fl)
                 )
              )
          (* --- Pstr_modtype --- *)
          (* sig S = MT *)
          | `infix (_, "=", `seq [ `id "sig"; `id name ], mt_fl) ->
            Ml.Str.modtype ~loc
              (Ml.Mtd.mk ~typ:(E_module_type.eval mt_fl) (Ml.mkloc loc name))
          (* --- Pstr_open --- *)
          | `seq [ `id "open"; me_fl ] ->
            Ml.Str.open_ ~loc (Ml.Opn.mk (E_module_expr.eval me_fl))
          | `seq [ `postfix (_, "!", `id "open"); me_fl ] ->
            Ml.Str.open_ ~loc
              (Ml.Opn.mk ~override:Asttypes.Override (E_module_expr.eval me_fl))
          (* --- Pstr_include --- *)
          | `seq [ `id "include"; me_fl ] ->
            Ml.Str.include_ ~loc (Ml.Incl.mk (E_module_expr.eval me_fl))
          (* --- Pstr_attribute --- *)
          | `brackets (`at (`id name, payload_fl)) ->
            Ml.Str.attribute ~loc (E_attribute.eval name payload_fl)
          (* --- Pstr_extension --- *)
          | `prefix (_, "%", `id name) ->
            Ml.Str.extension ~loc (Ml.mkloc loc name, Parsetree.PStr [])
          | exp_fl ->
            let exp_ml = E_expression.eval exp_fl in
            Ml.Str.eval ~loc exp_ml
        )
      )
    )
end

module E_structure = struct
  let eval (fl : Flx.t) =
    match fl with
    | `semi items_fl -> List.map E_structure_item.eval items_fl
    | item_fl -> [ E_structure_item.eval item_fl ]
end

let parse_fl ?file_name chan =
  let lex = Flx.Lex.read_channel ?file_name chan in
  let fl = Flx.parse lex in
  let str_ml = E_structure.eval fl in
  str_ml

let parse_ml ?file_name:_ chan =
  let lexbuf = Lexing.from_channel chan in
  let str_ml = Parse.implementation lexbuf in
  str_ml

let write_ml = Pprintast.structure
let write_sexp = Printsexp.structure 0
let write_ast = Printast.structure 0

let run ~input_format ~output_format ?file_name chan =
  let parse_ast =
    match input_format with
    | `ml -> parse_ml
    | `fl -> parse_fl
    | `sexp -> failwith "TODO"
    | `ast -> failwith "TODO"
  in
  let print_ast =
    match output_format with
    | `ml -> write_ml
    | `fl -> failwith "TODO"
    | `sexp -> write_sexp
    | `ast -> write_ast
  in
  let str_ml = parse_ast ?file_name chan in
  print "%a" print_ast str_ml

let format_of_string = function
  | "fl" -> Some `fl
  | "ml" -> Some `ml
  | "sexp" -> Some `sexp
  | "ast" -> Some `ast
  | _ -> None

(* let string_of_format = function *)
(*   | `fl -> "fl" *)
(*   | `ml -> "ml" *)
(*   | `sexp -> "sexp" *)
(*   | `ast -> "ast" *)

module Args = struct
  let input_format = ref "fl"
  let output_format = ref "ml"
  let input_files = ref []
  let anon filename = input_files := filename :: !input_files

  let all =
    [
      ("-i", Arg.Set_string input_format, "Input format");
      ("-o", Arg.Set_string output_format, "Output format");
    ]
end

let () =
  Printexc.record_backtrace true;
  Arg.parse Args.all Args.anon "fl [-i <input>] [-o <output>] <file>...";
  let input_format = format_of_string !Args.input_format |> Option.get in
  let output_format = format_of_string !Args.output_format |> Option.get in
  match !Args.input_files with
  | [ file_name ] ->
    In_channel.with_open_text file_name (fun chan ->
        run ~input_format ~output_format ~file_name chan
    )
  | [] -> run ~input_format ~output_format stdin
  | _ -> failwith "too many input files"
