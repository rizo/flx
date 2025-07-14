open struct
  module Ast = Astlib.Ast_503

  module Fmt = struct
    let pf = Format.fprintf
    let pr = Format.printf
    let epr = Format.eprintf
    let list ?sep pp l = Format.pp_print_list ?pp_sep:sep pp l
    let sp = Format.pp_print_space
  end

  let print ?(break = Format.pp_print_newline) fmt =
    Format.kfprintf (fun f -> break f ()) Format.std_formatter fmt

  let todo () = failwith "TODO"

  let wip what flx =
    Format.eprintf "WIP: %s: %a@." what Flx.pp flx;
    exit 42
end

(* Assert AST type *)
let _ : char -> Ast.Parsetree.constant = Ast_helper.Const.char

module Ml = struct
  type structure = Ast.Parsetree.structure
  type pattern = Ast.Parsetree.pattern
  type expression = Ast.Parsetree.expression
  type structure_item = Ast.Parsetree.structure_item
  type core_type = Ast.Parsetree.core_type
  type value_constraint = Ast.Parsetree.value_constraint

  module Vc = struct
    let constr vars typ =
      Ast.Parsetree.Pvc_constraint { locally_abstract_univars = vars; typ }
  end

  include Ast_helper
end

let loc = Location.none
let mknoloc = Location.mknoloc
let mkloc loc x = Location.mkloc x loc

module Eval_typ = struct
  let eval (fl : Flx.t) : Ml.core_type =
    match fl with
    (* a *)
    | `id id -> Ml.Typ.constr ~loc (mkloc loc (Longident.Lident id)) []
    | _ -> wip "typ" fl
end

module Eval_vc = struct
  let eval (fl : Flx.t) : Ml.value_constraint =
    match fl with
    | typ_fl ->
      let typ_ml = Eval_typ.eval typ_fl in
      Ml.Vc.constr [] typ_ml
end

module Eval_exp = struct
  let eval (fl : Flx.t) : Ml.expression =
    match fl with
    | `id id -> Ml.Exp.ident ~loc (mknoloc (Longident.Lident id))
    | `int int -> Ml.Exp.constant (Ml.Const.int int)
    | _ -> todo ()
end

module Eval_pat = struct
  let eval (fl : Flx.t) : Ml.pattern =
    match fl with
    | `id id -> Ml.Pat.var ~loc (mknoloc id)
    | `int int -> Ml.Pat.constant (Ml.Const.int int)
    | _ -> todo ()
end

module Eval_vb = struct
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
    | _ -> todo ()
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

  let eval (fl : Flx.t) : Ml.structure_item =
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
  match Sys.argv with
  | [| _; file_name |] ->
    In_channel.with_open_text file_name (fun chan -> run ~file_name chan)
  | [| _ |] -> run stdin
  | _ -> usage ()
