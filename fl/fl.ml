open struct
  module Parsetree = Astlib.Ast_503.Parsetree
  module Asttypes = Astlib.Ast_503.Asttypes
end

open Prelude

(* Assert current AST version: [Ast] must match [Ast_helper]. *)
let _ : char -> Parsetree.constant = Ast_helper.Const.char

module Ml = struct
  module Attr = Ast_helper.Attr
  module Const = Ast_helper.Const
  module Exp = Ast_helper.Exp
  module Pat = Ast_helper.Pat
  module Str = Ast_helper.Str
  module Typ = Ast_helper.Typ
  module Type = Ast_helper.Type
  module Vb = Ast_helper.Vb

  module Vc = struct
    let constraint_ vars typ =
      Parsetree.Pvc_constraint { locally_abstract_univars = vars; typ }
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
    let mk ?guard:pc_guard pc_lhs pc_rhs =
      { Parsetree.pc_lhs; pc_guard; pc_rhs }
  end

  let mknoloc = Location.mknoloc
  let mkloc loc x = Location.mkloc x loc
  let ident_noloc xs = mknoloc (Option.get (Longident.unflatten xs))
  let ident ~loc xs = mkloc loc (Option.get (Longident.unflatten xs))
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
  let rec eval (fl : Flx.t) =
    match fl with
    | `id "_" -> Ml.Typ.any ~loc ()
    | `id id when is_upper_name id -> Ml.Typ.var ~loc (String.lowercase_ascii id)
    | `id id -> Ml.Typ.constr ~loc (Ml.ident_noloc [ id ]) []
    | `dot path_id ->
      let path_id =
        List.map
          (function
            | `id id -> id
            | _ -> failwith "unexpected dot segment"
            )
          path_id
      in
      Ml.Typ.constr ~loc (Ml.ident ~loc path_id) []
    | `parens (`comma items) -> Ml.Typ.tuple ~loc (List.map eval items)
    | `parens typ -> eval typ
    | `infix ("->", left, right) ->
      Ml.Typ.arrow ~loc Asttypes.Nolabel (eval left) (eval right)
    | _ -> wip "typ" fl
end

and E_value_constraint : sig
  val eval : Flx.t -> Parsetree.value_constraint
end = struct
  let eval (fl : Flx.t) =
    match fl with
    | typ_fl ->
      let typ_ml = E_core_type.eval typ_fl in
      Ml.Vc.constraint_ [] typ_ml
end

and Eval_case : sig
  val eval : Flx.t -> Parsetree.case
end = struct
  let eval (fl : Flx.t) =
    match fl with
    | `infix ("->", pat, exp) ->
      let pat_ml = E_pattern.eval pat in
      let exp_ml = E_expression.eval exp in
      Ml.Case.mk pat_ml exp_ml
    | _ -> wip "case" fl
end

and E_expression : sig
  val eval : Flx.t -> Parsetree.expression
  val eval_function_param : Flx.t -> Parsetree.function_param
end = struct
  let eval_unit () = Ml.Exp.construct ~loc (Ml.ident_noloc [ "()" ]) None

  let eval_function_param fl =
    match fl with
    | `prefix ("~", `id label) ->
      let pat = Ml.Pat.var ~loc (Ml.mkloc loc label) in
      Ml.Fun.val_param ~label:(Asttypes.Labelled label) pat
    | _ ->
      let pat = E_pattern.eval fl in
      Ml.Fun.val_param pat

  let eval_apply_argument fl =
    match fl with
    (* ~a *)
    | `prefix ("~", `id lbl) ->
      (Asttypes.Labelled lbl, Ml.Exp.ident ~loc (Ml.ident ~loc [ lbl ]))
    (* TODO: more cases *)
    (* ~(a = b) *)
    (* a *)
    | arg_fl -> (Asttypes.Nolabel, E_expression.eval arg_fl)

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
    (* foo *)
    | `id id -> Ml.Exp.ident ~loc (Ml.ident_noloc [ id ])
    (* TODO: upper? *)
    (* TODO: ident vs construct *)
    | `dot path_id ->
      let path_id =
        List.map
          (function
            | `id id -> id
            | _ -> failwith "unexpected dot segment"
            )
          path_id
      in
      Ml.Exp.ident ~loc (Ml.ident ~loc path_id)
    | `parens (`seq []) -> eval_unit ()
    (* (items...,) _ *)
    | `parens (`comma items) ->
      Ml.Exp.tuple ~loc (List.map E_expression.eval items)
    (* (_) *)
    | `parens exp -> E_expression.eval exp
    (* --- Pexp_let --- *)
    | `comma [ `infix ("=", `seq [ `id "let"; pat_fl ], exp_fl); body_fl ] ->
      let vb =
        let pat_ml = E_pattern.eval pat_fl in
        let exp_ml = E_expression.eval exp_fl in
        Ml.Vb.mk pat_ml exp_ml
      in
      let body = E_expression.eval body_fl in
      Ml.Exp.let_ Nonrecursive [ vb ] body
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
    (* --- Pexp_function --- *)
    | `infix ("->", `seq (`id "fn" :: params_fl), body_fl) ->
      let params_ml = List.map eval_function_param params_fl in
      let body_ml = E_expression.eval body_fl in
      Ml.Exp.function_ ~loc params_ml None (Ml.Fun.body body_ml)
    (* fn { cases..., } *)
    | `seq [ `id "fn"; `braces (`comma cases) ] ->
      Ml.Exp.function_ ~loc [] None
        (Ml.Fun.cases (List.map Eval_case.eval cases))
    (* | `infix ("->", `seq (`id "fn" :: args), body) -> *)
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
          `parens (`infix ("=", pat_fl, `seq [ start_fl; `id "to"; stop_fl ]));
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
          `parens
            (`infix ("=", pat_fl, `seq [ start_fl; `id "downto"; stop_fl ]));
          `braces body_fl;
        ] ->
      let pat_ml = E_pattern.eval pat_fl in
      let start_ml = E_expression.eval start_fl in
      let stop_ml = E_expression.eval stop_fl in
      let body_ml = E_expression.eval body_fl in
      Ml.Exp.for_ ~loc pat_ml start_ml stop_ml Asttypes.Downto body_ml
    (* --- Pexp_assert --- *)
    (* assert exp *)
    | `seq [ `id "assert"; exp_fl ] ->
      let exp_ml = E_expression.eval exp_fl in
      Ml.Exp.assert_ ~loc exp_ml
    | `seq (f_fl :: args_fl) ->
      let f_ml = E_expression.eval f_fl in
      let args_ml = List.map eval_apply_argument args_fl in
      Ml.Exp.apply ~loc f_ml args_ml
    | `infix (op, e1_fl, e2_fl) ->
      let f_ml = Ml.Exp.ident ~loc (Ml.ident_noloc [ op ]) in
      let e1_ml = E_expression.eval e1_fl in
      let e2_ml = E_expression.eval e2_fl in
      Ml.Exp.apply ~loc f_ml
        [ (Asttypes.Nolabel, e1_ml); (Asttypes.Nolabel, e2_ml) ]
    (* -a *)
    | `prefix (op, e1_fl) ->
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
    | _ -> wip "exp" fl
end

and E_pattern : sig
  val eval : Flx.t -> Parsetree.pattern
end = struct
  let eval (fl : Flx.t) =
    match fl with
    (* 5 *)
    | `int x -> Ml.Pat.constant (Ml.Const.int x)
    (* "abc" *)
    | `str x -> Ml.Pat.constant (Ml.Const.string x)
    (* 'x' *)
    | `char x -> Ml.Pat.constant (Ml.Const.char x)
    (* () *)
    | `parens (`seq []) -> Ml.Pat.construct ~loc (Ml.ident_noloc [ "()" ]) None
    (* C *)
    | `id id when is_upper_name id ->
      Ml.Pat.construct ~loc (Ml.ident_noloc [ id ]) None
    (* C args... *)
    | `seq (`id id :: args_ml) when is_upper_name id ->
      let types_list = [] in
      let args_ml = Ml.Pat.tuple ~loc (List.map E_pattern.eval args_ml) in
      Ml.Pat.construct ~loc (Ml.ident_noloc [ id ]) (Some (types_list, args_ml))
    (* _ *)
    | `id "_" -> Ml.Pat.any ~loc ()
    (* a *)
    | `id id -> Ml.Pat.var ~loc (Ml.mknoloc id)
    (* (1, 'x', a) *)
    | `parens (`comma items) -> Ml.Pat.tuple ~loc (List.map E_pattern.eval items)
    (* (_) *)
    | `parens pat -> E_pattern.eval pat
    (* _ | _ *)
    | `pipe items ->
      List.fold_left
        (fun acc pat -> Ml.Pat.or_ ~loc acc pat)
        (E_pattern.eval (List.hd items))
        (List.map E_pattern.eval (List.tl items))
    (* x @ _ *)
    | `infix ("@", `id alias, pat_fl) ->
      let pat_ml = E_pattern.eval pat_fl in
      Ml.Pat.alias pat_ml (Ml.mkloc loc alias)
    (* err: x @ _ *)
    | `infix ("@", _, _) ->
      fail "invalid pattern alias: alias must be an identifier"
    | `seq [ item ] -> E_pattern.eval item
    | _ -> wip "pat" fl
end

and E_value_binding : sig
  val eval : Flx.t -> Parsetree.value_binding
end = struct
  let eval (fl : Flx.t) =
    match fl with
    (* a : int = 3 *)
    | `infix ("=", `infix (":", pat_fl, vc_fl), exp_fl) ->
      let pat_ml = E_pattern.eval pat_fl in
      let vc_ml = E_value_constraint.eval vc_fl in
      let exp_ml = E_expression.eval exp_fl in
      Ml.Vb.mk ~value_constraint:vc_ml pat_ml exp_ml
    (* a = 3 *)
    | `infix ("=", pat_fl, exp_fl) ->
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
    | `id name when is_upper_name name ->
      Ml.Type.constructor ~loc (Ml.mkloc loc name)
    (* C ... *)
    | `seq (`id name :: args_fl) when is_upper_name name ->
      let args_ml = List.map E_core_type.eval args_fl in
      Ml.Type.constructor ~loc ~args:(Parsetree.Pcstr_tuple args_ml)
        (Ml.mkloc loc name)
    | _ -> wip "constructor_declaration" fl
end

module E_structure_item = struct
  let eval_type_param fl =
    match fl with
    | `id "_" ->
      let t = Ml.Typ.any ~loc () in
      (t, (Asttypes.NoVariance, Asttypes.NoInjectivity))
    | `id param_id ->
      let t = Ml.Typ.var ~loc (String.lowercase_ascii param_id) in
      (t, (Asttypes.NoVariance, Asttypes.NoInjectivity))
    | fl -> wip "type_param" fl

  let eval_type_kind fl =
    match fl with
    (* { _ | _ | ... } *)
    | `braces (`pipe constructors) ->
      Parsetree.Ptype_variant
        (List.map E_constructor_declaration.eval constructors)
    (* { _ } *)
    | `braces constructor ->
      Parsetree.Ptype_variant [ E_constructor_declaration.eval constructor ]
    | _ -> Parsetree.Ptype_abstract

  let eval (fl : Flx.t) : Parsetree.structure_item =
    match fl with
    (* --- Pstr_type --- *)
    (* type t *)
    | `seq [ `id "type"; `id type_id ] ->
      let type_ = Ml.Type.mk (Ml.mkloc loc type_id) in
      Ml.Str.type_ Asttypes.Nonrecursive [ type_ ]
    (* type t[A, B] *)
    | `seq [ `id "type"; `id type_id; `brackets (`comma params_fl) ] ->
      let params = List.map eval_type_param params_fl in
      let type_ = Ml.Type.mk ~params (Ml.mkloc loc type_id) in
      Ml.Str.type_ Asttypes.Nonrecursive [ type_ ]
    (* type t[A] *)
    | `seq [ `id "type"; `id type_id; `brackets param_fl ] ->
      let params = [ eval_type_param param_fl ] in
      let type_ = Ml.Type.mk ~params (Ml.mkloc loc type_id) in
      Ml.Str.type_ Asttypes.Nonrecursive [ type_ ]
    (* type t = _ *)
    | `infix ("=", `seq [ `id "type"; `id type_id ], body_fl) ->
      let params = [] in
      let kind = eval_type_kind body_fl in
      let type_ = Ml.Type.mk ~params ~kind (Ml.mkloc loc type_id) in
      Ml.Str.type_ Asttypes.Recursive [ type_ ]
    (* type t[A, B] = _ *)
    | `infix
        ( "=",
          `seq [ `id "type"; `id type_id; `brackets (`comma params_fl) ],
          body_fl
        ) ->
      let params = List.map eval_type_param params_fl in
      let kind = eval_type_kind body_fl in
      let type_ = Ml.Type.mk ~params ~kind (Ml.mkloc loc type_id) in
      Ml.Str.type_ Asttypes.Recursive [ type_ ]
    (* --- Pstr_value --- *)
    (* val f args... = body *)
    | `infix ("=", `seq (`id "val" :: `id name :: args_fl), body_exp_fl) ->
      let params_ml = List.map E_expression.eval_function_param args_fl in
      let vb_pat = Ml.Pat.var ~loc (Ml.mkloc loc name) in
      let body_exp_ml = E_expression.eval body_exp_fl in
      let body_ml = Ml.Fun.body body_exp_ml in
      let vb_exp = Ml.Exp.function_ ~loc params_ml None body_ml in
      let vb = Ml.Vb.mk vb_pat vb_exp in
      Ml.Str.value ~loc Asttypes.Nonrecursive [ vb ]
    (* val a = 1 *)
    | `infix ("=", `seq (`id "val" :: [ vb_pat_fl ]), vb_exp_fl) ->
      let vb_pat_ml = E_pattern.eval vb_pat_fl in
      let vb_exp_ml = E_expression.eval vb_exp_fl in
      let vb_ml = Ml.Vb.mk vb_pat_ml vb_exp_ml in
      Ml.Str.value ~loc Asttypes.Nonrecursive [ vb_ml ]
    (* val a : int = 1 *)
    | `infix ("=", `infix (":", `seq (`id "val" :: [ pat_fl ]), vc_fl), exp_fl)
      ->
      let pat_ml = E_pattern.eval pat_fl in
      let vc_ml = E_value_constraint.eval vc_fl in
      let exp_ml = E_expression.eval exp_fl in
      let vb_ml = Ml.Vb.mk ~value_constraint:vc_ml pat_ml exp_ml in
      Ml.Str.value ~loc Asttypes.Nonrecursive [ vb_ml ]
    (* val { a = 1, b = 2 } *)
    | `seq [ `id "val"; `braces (`comma vbl_fl) ] ->
      let vbl_ml = List.map E_value_binding.eval vbl_fl in
      Ml.Str.value ~loc Asttypes.Nonrecursive vbl_ml
    | exp_fl ->
      let exp_ml = E_expression.eval exp_fl in
      Ml.Str.eval ~loc exp_ml
end

module E_structure = struct
  let eval (fl : Flx.t) =
    match fl with
    | `semi items_fl -> List.map E_structure_item.eval items_fl
    | item_fl -> [ E_structure_item.eval item_fl ]
end

let run ?file_name chan =
  let lex = Flx.Lex.read_channel ?file_name chan in
  let fl = Flx.parse lex in
  let str_ml = E_structure.eval fl in
  print "%a" Pprintast.structure str_ml
(* print "%a" Printast.implementation str_ml *)

let usage () =
  prerr_endline "usage: fl [input]";
  exit 1

let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [| _; file_name |] ->
    In_channel.with_open_text file_name (fun chan -> run ~file_name chan)
  | [| _ |] -> run stdin
  | _ -> usage ()
