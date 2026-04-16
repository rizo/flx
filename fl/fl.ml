open struct
  module Ast = Astlib.Ast_503
end

open Prelude

(* Assert current AST version: [Ast] must match [Ast_helper]. *)
let _ : char -> Ast.Parsetree.constant = Ast_helper.Const.char

module Ml = struct
  type attr = Ast.Parsetree.attribute
  type const = Ast.Parsetree.constant
  type case = Ast.Parsetree.case
  type exp = Ast.Parsetree.expression
  type pat = Ast.Parsetree.pattern
  type str = Ast.Parsetree.structure_item
  type typ = Ast.Parsetree.core_type
  type vc = Ast.Parsetree.value_constraint
  type vb = Ast.Parsetree.value_binding

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
      Ast.Parsetree.Pvc_constraint { locally_abstract_univars = vars; typ }
  end

  module Case = struct
    let mk ?guard:pc_guard pc_lhs pc_rhs =
      { Ast.Parsetree.pc_lhs; pc_guard; pc_rhs }
  end
end

let loc = Location.none
let mknoloc = Location.mknoloc
let mkloc loc x = Location.mkloc x loc
let ident xs = Option.get (Longident.unflatten xs)
let ident_noloc xs = mknoloc (Option.get (Longident.unflatten xs))

module rec Eval_typ : sig
  val eval : Flx.t -> Ml.typ
end = struct
  let eval (fl : Flx.t) : Ml.typ =
    match fl with
    (* a *)
    | `id id -> Ml.Typ.constr ~loc (mkloc loc (Longident.Lident id)) []
    | _ -> wip "typ" fl
end

and Eval_vc : sig
  val eval : Flx.t -> Ml.vc
end = struct
  let eval (fl : Flx.t) =
    match fl with
    | typ_fl ->
      let typ_ml = Eval_typ.eval typ_fl in
      Ml.Vc.constraint_ [] typ_ml
end

and Eval_case : sig
  val eval : Flx.t -> Ml.case
end = struct
  let eval (fl : Flx.t) =
    match fl with
    | `infix ("->", pat, exp) ->
      let pat_ml = Eval_pat.eval pat in
      let exp_ml = Eval_exp.eval exp in
      Ml.Case.mk pat_ml exp_ml
    | _ -> wip "case" fl
end

and Eval_exp : sig
  val eval : Flx.t -> Ml.exp
end = struct
  let eval (fl : Flx.t) =
    match fl with
    | `id id -> Ml.Exp.ident ~loc (mknoloc (Longident.Lident id))
    | `int x -> Ml.Exp.constant (Ml.Const.int x)
    | `str x -> Ml.Exp.constant (Ml.Const.string x)
    | `parens (`seq []) -> Ml.Exp.construct (ident_noloc [ "()" ]) None
    | `seq [ `id "match"; exp; `braces (`comma cases) ] ->
      let exp_ml = Eval_exp.eval exp in
      Ml.Exp.match_ exp_ml (List.map Eval_case.eval cases)
    | `seq [ `id "match"; exp; `braces case ] ->
      let exp_ml = Eval_exp.eval exp in
      Ml.Exp.match_ exp_ml [ Eval_case.eval case ]
    | `seq [ `id "match"; exp; _ ] -> fail "missing braces around match cases"
    | _ -> wip "exp" fl
end

and Eval_pat : sig
  val eval : Flx.t -> Ml.pat
end = struct
  let eval (fl : Flx.t) =
    match fl with
    | `id id -> Ml.Pat.var ~loc (mknoloc id)
    | `int x -> Ml.Pat.constant (Ml.Const.int x)
    | `str x -> Ml.Pat.constant (Ml.Const.string x)
    | `parens (`seq []) -> Ml.Pat.construct (ident_noloc [ "()" ]) None
    | `parens pat -> Eval_pat.eval pat
    | `infix ("@", `id alias, pat_fl) ->
      let pat_ml = Eval_pat.eval pat_fl in
      Ml.Pat.alias pat_ml (mknoloc alias)
    | `infix ("@", _, _) ->
      fail "invalid pattern alias: alias must be an identifier"
    (* | `infix ("..", `int _, pat_fl) -> *)
    (*   let pat_ml = Eval_pat.eval pat_fl in *)
    (*   Ml.Pat.alias pat_ml (mknoloc alias) *)
    | _ -> wip "pat" fl
end

and Eval_vb : sig
  val eval : Flx.t -> Ml.vb
end = struct
  let eval (fl : Flx.t) =
    match fl with
    (* a : int = 3 *)
    | `infix ("=", `infix (":", pat_fl, vc_fl), exp_fl) ->
      let pat_ml = Eval_pat.eval pat_fl in
      let vc_ml = Eval_vc.eval vc_fl in
      let exp_ml = Eval_exp.eval exp_fl in
      Ml.Vb.mk ~value_constraint:vc_ml pat_ml exp_ml
    (* a = 3 *)
    | `infix ("=", pat_fl, exp_fl) ->
      let pat_ml = Eval_pat.eval pat_fl in
      let exp_ml = Eval_exp.eval exp_fl in
      Ml.Vb.mk pat_ml exp_ml
    | _ -> wip "vb" fl
end

module Eval_stri = struct
  let stri_dummy = Ml.Str.value Ast.Asttypes.Nonrecursive []

  let eval_typ_param fl =
    match fl with
    | `id "_" ->
      let t = Ml.Typ.any ~loc () in
      (t, (Asttypes.NoVariance, Asttypes.NoInjectivity))
    | `id param_id ->
      let t = Ml.Typ.var ~loc (String.lowercase_ascii param_id) in
      (t, (Asttypes.NoVariance, Asttypes.NoInjectivity))
    | fl ->
      print ">>> %a@." Flx.pp fl;
      failwith "invalid type param"

  let eval (fl : Flx.t) : Ml.str =
    match fl with
    (* val a = 1 *)
    | `infix ("=", `seq (`id "val" :: [ vb_pat_fl ]), vb_exp_fl) ->
      let vb_pat_ml = Eval_pat.eval vb_pat_fl in
      let vb_exp_ml = Eval_exp.eval vb_exp_fl in
      let vb_ml = Ml.Vb.mk vb_pat_ml vb_exp_ml in
      Ml.Str.value ~loc Ast.Asttypes.Nonrecursive [ vb_ml ]
    (* val a : int = 1 *)
    | `infix ("=", `infix (":", `seq (`id "val" :: [ pat_fl ]), vc_fl), exp_fl)
      ->
      let pat_ml = Eval_pat.eval pat_fl in
      let vc_ml = Eval_vc.eval vc_fl in
      let exp_ml = Eval_exp.eval exp_fl in
      let vb_ml = Ml.Vb.mk ~value_constraint:vc_ml pat_ml exp_ml in
      Ml.Str.value ~loc Ast.Asttypes.Nonrecursive [ vb_ml ]
    (* val { a = 1, b = 2 } *)
    | `seq [ `id "val"; `braces (`comma vbl_fl) ] ->
      let vbl_ml = List.map Eval_vb.eval vbl_fl in
      Ml.Str.value ~loc Ast.Asttypes.Nonrecursive vbl_ml
    (* type a *)
    | `seq [ `id "type"; `id type_id ] ->
      let type_ = Ml.Type.mk (mkloc loc type_id) in
      Ml.Str.type_ Ast.Asttypes.Nonrecursive [ type_ ]
    (* type t[A, B] *)
    | `seq [ `id "type"; `id type_id; `brackets (`comma params_fl) ] ->
      let params = List.map eval_typ_param params_fl in
      let type_ = Ml.Type.mk ~params (mkloc loc type_id) in
      Ml.Str.type_ Ast.Asttypes.Nonrecursive [ type_ ]
    (* type t[A] *)
    | `seq [ `id "type"; `id type_id; `brackets param_fl ] ->
      let params = [ eval_typ_param param_fl ] in
      let type_ = Ml.Type.mk ~params (mkloc loc type_id) in
      Ml.Str.type_ Ast.Asttypes.Nonrecursive [ type_ ]
    | exp_fl ->
      let exp_ml = Eval_exp.eval exp_fl in
      Ml.Str.eval ~loc exp_ml
end

module Eval_str = struct
  let eval (fl : Flx.t) =
    match fl with
    | `semi items_fl -> List.map Eval_stri.eval items_fl
    | _ -> [ Eval_stri.eval fl ]
end

let run ?file_name chan =
  let lex = Flx.Lex.read_channel ?file_name chan in
  let fl = Flx.parse lex in
  let str_ml = Eval_str.eval fl in
  print "%a" Pprintast.structure str_ml

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
