((pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_char 'x')))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_string "hello" None)))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_any")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc Ptyp_any) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_var")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_arrow1")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_arrow Nolabel
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
         ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
          ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_arrow2")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_arrow Nolabel
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
         ((ptyp_desc
          (Ptyp_arrow Nolabel
           ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
            ()))
           ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes
            ()))))
          (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_arrow3")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_arrow Nolabel
         ((ptyp_desc
          (Ptyp_arrow Nolabel
           ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
           ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes
            ()))))
          (ptyp_attributes ()))
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_tuple1")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
           ())))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_tuple2")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc
           (Ptyp_tuple
            (((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
              ()))
             ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes
              ())))))
           (ptyp_attributes ())))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_constr1")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_constr2")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_constr (Ldot (Lident "M") "t") ())) (ptyp_attributes
        ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "typ_constr3")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_constr (Ldot (Ldot (Lident "M1") "M2") "t") ()))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pc_guard
       None) (pc_rhs
       ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_alias
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "2" None)))))
          (ppat_attributes ()))
         "x"))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc (Ppat_constant ((pconst_desc (Pconst_char 'a')))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc
        (Ppat_constant ((pconst_desc (Pconst_string "abc" None)))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ()))))
      ((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "3" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_tuple
         (((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
          ((ppat_desc (Ppat_var "b")) (ppat_attributes ())))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_tuple
         (((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
          ((ppat_desc (Ppat_var "b")) (ppat_attributes ()))
          ((ppat_desc (Ppat_var "c")) (ppat_attributes ())))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_construct (Lident "None") None)) (ppat_attributes
        ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
      ((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_construct (Lident "C")
         (Some
          (()
           ((ppat_desc
            (Ppat_tuple
             (((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
              ((ppat_desc (Ppat_var "b")) (ppat_attributes ())))))
            (ppat_attributes ()))))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
      ((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_or
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
          (ppat_attributes ()))
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "2" None)))))
          (ppat_attributes ()))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
      ((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_ident (Ldot (Lident "X") "a"))) (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_ident (Ldot (Ldot (Lident "X1") "X2") "a")))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_char 'x')))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_string "hello" None)))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_let Nonrecursive
     (((pvb_pat ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))) (pvb_expr
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ())))
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
          (pexp_attributes ()))))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     ((pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_var "x")) (ppat_attributes ())))))
     None
     (Pfunction_body
      ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     ((pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))))
      (pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_var "y")) (ppat_attributes ())))))
     None
     (Pfunction_body
      ((pexp_desc
       (Pexp_apply
        ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
        ((Nolabel
          ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
         (Nolabel
          ((pexp_desc (Pexp_ident (Lident "y"))) (pexp_attributes ()))))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     ((pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_construct (Lident "()") None)) (ppat_attributes
         ())))))
     None
     (Pfunction_body
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     ((pparam_desc
       (Pparam_val (Labelled "x") None
        ((ppat_desc (Ppat_var "x")) (ppat_attributes ())))))
     None
     (Pfunction_body
      ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function () None
     (Pfunction_cases
      (((pc_lhs
        ((ppat_desc
         (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
         (ppat_attributes ())))
        (pc_guard None) (pc_rhs
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
         (pexp_attributes ()))))
       ((pc_lhs ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pc_guard
        None) (pc_rhs
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ())))))
      ())))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
     ((Nolabel ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
     ((Nolabel ((pexp_desc (Pexp_ident (Lident "x1"))) (pexp_attributes ())))
      (Nolabel ((pexp_desc (Pexp_ident (Lident "x2"))) (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
     (((Labelled "x1")
       ((pexp_desc (Pexp_ident (Lident "x1"))) (pexp_attributes ())))
      (Nolabel ((pexp_desc (Pexp_ident (Lident "x2"))) (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
     (((Labelled "x1")
       ((pexp_desc (Pexp_ident (Lident "x1"))) (pexp_attributes ())))
      ((Labelled "x2")
       ((pexp_desc (Pexp_ident (Lident "x2"))) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc (Pexp_construct (Lident "()") None)) (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "~-"))) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
     ((Nolabel ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
     ((Nolabel ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc
        (Pexp_apply
         ((pexp_desc (Pexp_ident (Lident "~-"))) (pexp_attributes ()))
         ((Nolabel
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
            (pexp_attributes ()))))))
        (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pc_guard
       None) (pc_rhs
       ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
      ((pc_lhs ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pc_guard
       None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc
        (Ppat_alias
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "2" None)))))
          (ppat_attributes ()))
         "x"))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "e"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_or
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
          (ppat_attributes ()))
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "2" None)))))
          (ppat_attributes ()))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
      ((pc_lhs ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pc_guard
       None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
          (pexp_attributes ())))
        (Nolabel
         ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))))))
      (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_or
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
          (ppat_attributes ()))
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "2" None)))))
          (ppat_attributes ()))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
      ((pc_lhs ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pc_guard
       None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
      ((pexp_desc (Pexp_ident (Lident "b"))) (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "3" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "3" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "4" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "5" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "6" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "7" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "8" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "9" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "10" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
       (pexp_attributes ()))
      ((pexp_desc
       (Pexp_tuple
        (((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
         ((pexp_desc (Pexp_ident (Lident "b"))) (pexp_attributes ())))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_construct (Lident "()") None)) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_construct (Lident "[]") None)) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_construct (Lident "false") None)) (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval ((pexp_desc (Pexp_variant "Green" None)) (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_variant "Rgb"
     (Some
      ((pexp_desc
       (Pexp_tuple
        (((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "255" None)))))
          (pexp_attributes ()))
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
          (pexp_attributes ()))
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
          (pexp_attributes ())))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     (((Lident "x")
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
     None))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     (((Lident "x")
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
      ((Lident "y")
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ()))))
     None))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     (((Lident "x")
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ())))
      ((Lident "z")
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
     (Some ((pexp_desc (Pexp_ident (Lident "p"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     (((Lident "x")
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ()))))
     (Some ((pexp_desc (Pexp_ident (Lident "p"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval ((pexp_desc (Pexp_array ())) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "3" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_ifthenelse
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident ">"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
          (pexp_attributes ()))))))
      (pexp_attributes ()))
     ((pexp_desc (Pexp_ident (Lident "e1"))) (pexp_attributes ()))
     (Some ((pexp_desc (Pexp_ident (Lident "e2"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "3" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_while
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident ">"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
          (pexp_attributes ()))))))
      (pexp_attributes ()))
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident "print"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_for ((ppat_desc (Ppat_var "i")) (ppat_attributes ()))
     ((pexp_desc (Pexp_ident (Lident "e1"))) (pexp_attributes ()))
     ((pexp_desc (Pexp_ident (Lident "e2"))) (pexp_attributes ())) Upto
     ((pexp_desc
      (Pexp_array
       (((pexp_desc (Pexp_ident (Lident "e3"))) (pexp_attributes ())))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_for ((ppat_desc (Ppat_var "i")) (ppat_attributes ()))
     ((pexp_desc (Pexp_ident (Lident "e1"))) (pexp_attributes ()))
     ((pexp_desc (Pexp_ident (Lident "e2"))) (pexp_attributes ())) Downto
     ((pexp_desc (Pexp_ident (Lident "e3"))) (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_for ((ppat_desc (Ppat_var "i")) (ppat_attributes ()))
     ((pexp_desc (Pexp_ident (Lident "e1"))) (pexp_attributes ()))
     ((pexp_desc (Pexp_ident (Lident "e2"))) (pexp_attributes ())) Upto
     ((pexp_desc
      (Pexp_array
       (((pexp_desc
         (Pexp_apply
          ((pexp_desc (Pexp_ident (Lident "print"))) (pexp_attributes ()))
          ((Nolabel
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_string "hello" None)))))
             (pexp_attributes ()))))))
         (pexp_attributes ()))
        ((pexp_desc
         (Pexp_apply
          ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
          ((Nolabel
            ((pexp_desc (Pexp_construct (Lident "()") None)) (pexp_attributes
             ()))))))
         (pexp_attributes ())))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_send ((pexp_desc (Pexp_ident (Lident "obj"))) (pexp_attributes ()))
     "meth1"))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_assert
     ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_assert
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident ">"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
          (pexp_attributes ()))))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_lazy
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_object
     ((pcstr_self ((ppat_desc Ppat_any) (ppat_attributes ()))) (pcstr_fields
      ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_object
     ((pcstr_self ((ppat_desc Ppat_any) (ppat_attributes ()))) (pcstr_fields
      (((pcf_desc
        (Pcf_method "a" Public
         (Cfk_concrete Fresh
          ((pexp_desc
           (Pexp_poly
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
             (pexp_attributes ()))
            None))
           (pexp_attributes ())))))
        (pcf_attributes ()))
       ((pcf_desc
        (Pcf_method "b" Public
         (Cfk_concrete Fresh
          ((pexp_desc
           (Pexp_poly
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
             (pexp_attributes ()))
            None))
           (pexp_attributes ())))))
        (pcf_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs ((pexp_desc Pexp_unreachable) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_type Nonrecursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     Ptype_abstract) (ptype_private Public) (ptype_manifest None)
     (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Nonrecursive
   (((ptype_name "t") (ptype_params
     ((((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ()))
       (NoVariance NoInjectivity))))
     (ptype_cstrs ()) (ptype_kind Ptype_abstract) (ptype_private Public)
     (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Nonrecursive
   (((ptype_name "t") (ptype_params
     ((((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ()))
       (NoVariance NoInjectivity))
      (((ptyp_desc (Ptyp_var "b")) (ptyp_attributes ()))
       (NoVariance NoInjectivity))))
     (ptype_cstrs ()) (ptype_kind Ptype_abstract) (ptype_private Public)
     (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Nonrecursive
   (((ptype_name "t") (ptype_params
     ((((ptyp_desc Ptyp_any) (ptyp_attributes ()))
       (NoVariance NoInjectivity))))
     (ptype_cstrs ()) (ptype_kind Ptype_abstract) (ptype_private Public)
     (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "A") (pcd_vars ()) (pcd_args (Pcstr_tuple ())) (pcd_res
        None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "A") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "A") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
           ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "A") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc
           (Ptyp_tuple
            (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
              ()))
             ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
              ())))))
           (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "A") (pcd_vars ()) (pcd_args (Pcstr_tuple ())) (pcd_res
        None) (pcd_attributes ()))
       ((pcd_name "B") (pcd_vars ()) (pcd_args (Pcstr_tuple ())) (pcd_res
        None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "A") (pcd_vars ()) (pcd_args (Pcstr_tuple ())) (pcd_res
        None) (pcd_attributes ()))
       ((pcd_name "B") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
      (Nolabel
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "f01")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc (Ppat_var "x")) (ppat_attributes ())))))
       None
       (Pfunction_body
        ((pexp_desc
         (Pexp_apply
          ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
          ((Nolabel
            ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
           (Nolabel
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
             (pexp_attributes ()))))))
         (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "v01")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat
     ((ppat_desc
      (Ppat_tuple
       (((ppat_desc (Ppat_var "v02")) (ppat_attributes ()))
        ((ppat_desc (Ppat_var "v03")) (ppat_attributes ())))))
      (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat
     ((ppat_desc
      (Ppat_alias
       ((ppat_desc (Ppat_construct (Lident "()") None)) (ppat_attributes ()))
       "v04"))
      (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "v05")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ()))
    ((pvb_pat ((ppat_desc (Ppat_var "v06")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_type Nonrecursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     Ptype_abstract) (ptype_private Public) (ptype_manifest None)
     (ptype_attributes ()))))))
