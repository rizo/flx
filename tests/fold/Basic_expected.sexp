((pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc Ptyp_any) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e_arrow_1")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc Ppat_any) (ppat_attributes ())))))
       None
       (Pfunction_body
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_string "abc" None)))))
         (pexp_attributes ())))))
      (pexp_attributes ())))
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
   (((pvb_pat ((ppat_desc (Ppat_var "e_arrow_2")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc Ppat_any) (ppat_attributes ()))))
        (pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc Ppat_any) (ppat_attributes ())))))
       None
       (Pfunction_body
        ((pexp_desc (Pexp_construct (Lident "()") None)) (pexp_attributes
         ())))))
      (pexp_attributes ())))
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
           ((ptyp_desc (Ptyp_constr (Lident "unit") ())) (ptyp_attributes
            ()))))
          (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e_arrow_3")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val (Labelled "l") None
          ((ppat_desc Ppat_any) (ppat_attributes ())))))
       None
       (Pfunction_body
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_string "abc" None)))))
         (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_arrow (Labelled "l")
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
         ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
          ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e_arrow_4")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val (Optional "l") None
          ((ppat_desc Ppat_any) (ppat_attributes ()))))
        (pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc (Ppat_construct (Lident "()") None)) (ppat_attributes
           ())))))
       None
       (Pfunction_body
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_string "abc" None)))))
         (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_arrow (Optional "l")
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
         ((ptyp_desc
          (Ptyp_arrow Nolabel
           ((ptyp_desc (Ptyp_constr (Lident "unit") ())) (ptyp_attributes
            ()))
           ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
            ()))))
          (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e_arrow_5")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc Ppat_any) (ppat_attributes ()))))
        (pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc Ppat_any) (ppat_attributes ())))))
       None
       (Pfunction_body
        ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
         ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_arrow Nolabel
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
         ((ptyp_desc
          (Ptyp_arrow Nolabel
           ((ptyp_desc
            (Ptyp_arrow Nolabel
             ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
              ()))
             ((ptyp_desc (Ptyp_constr (Lident "unit") ())) (ptyp_attributes
              ()))))
            (ptyp_attributes ()))
           ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes
            ()))))
          (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "ptyp_tuple_1")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_tuple
       (((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ()))
        ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
         ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "ptyp_tuple_2")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_tuple
       (((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ()))
        ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
         ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "ptyp_tuple_3")) (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_tuple
       (((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ()))
        ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
         ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_construct (Lident "None") None)) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_constr (Lident "option")
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_construct (Lident "Ok")
       (Some
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_constr (Lident "result")
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
           ())))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "ptyp_alias_1")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_alias
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
         "a"))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "ptyp_alias_2")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_alias
         ((ptyp_desc
          (Ptyp_variant (((prf_desc (Rtag "A" true ())) (prf_attributes ())))
           Open None))
          (ptyp_attributes ()))
         "a"))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "ptyp_variant_1")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_variant "A" None)) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_variant (((prf_desc (Rtag "A" true ())) (prf_attributes ())))
         Closed None))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "ptyp_variant_2")) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_variant "A" None)) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_variant
         (((prf_desc (Rtag "A" true ())) (prf_attributes ()))
          ((prf_desc (Rtag "B" true ())) (prf_attributes ())))
         Closed None))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_variant "A" None)) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_variant
         (((prf_desc (Rtag "A" true ())) (prf_attributes ()))
          ((prf_desc (Rtag "B" true ())) (prf_attributes ())))
         Open None))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_variant "A"
       (Some
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_variant
         (((prf_desc
           (Rtag "A" false
            (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
              ())))))
           (prf_attributes ())))
         Closed None))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_poly ("a")
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_poly ("a" "b")
         ((ptyp_desc
          (Ptyp_arrow Nolabel
           ((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ()))
           ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
          (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))) (pc_guard
       None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_alias
         ((ppat_desc
          (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
          (ppat_attributes ()))
         "a"))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_interval ((pconst_desc (Pconst_char 'a')))
         ((pconst_desc (Pconst_char 'z')))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_tuple
         (((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
          ((ppat_desc (Ppat_var "b")) (ppat_attributes ())))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_tuple
         (((ppat_desc (Ppat_var "l1")) (ppat_attributes ()))
          ((ppat_desc (Ppat_var "b")) (ppat_attributes ())))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_tuple
         (((ppat_desc (Ppat_var "l1")) (ppat_attributes ()))
          ((ppat_desc (Ppat_var "l2")) (ppat_attributes ())))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_construct (Lident "None") None)) (ppat_attributes
        ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_construct (Lident "Some")
         (Some (() ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc (Ppat_variant "A" None)) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_variant "B"
         (Some ((ppat_desc (Ppat_var "a")) (ppat_attributes ())))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_record
         (((Lident "x") ((ppat_desc (Ppat_var "x")) (ppat_attributes ())))
          ((Lident "y") ((ppat_desc (Ppat_var "y")) (ppat_attributes ()))))
         Closed))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_record
         (((Lident "x") ((ppat_desc (Ppat_var "x2")) (ppat_attributes ())))
          ((Lident "y") ((ppat_desc (Ppat_var "y")) (ppat_attributes ()))))
         Closed))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_record
         (((Lident "x") ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))))
         Open))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc (Ppat_array ())) (ppat_attributes ()))) (pc_guard
       None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_array
         (((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
          ((ppat_desc Ppat_any) (ppat_attributes ())))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_or ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
         ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_constraint ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
         ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc (Ppat_type (Lident "t1"))) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat
     ((ppat_desc
      (Ppat_lazy ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))))
      (ppat_attributes ())))
     (pvb_expr
     ((pexp_desc
      (Pexp_lazy
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_unpack None)) (ppat_attributes ())))
     (pvb_expr ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc (Ppat_unpack (Some "M"))) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc
        (Ppat_exception ((ppat_desc (Ppat_var "err")) (ppat_attributes ()))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_open (Lident "M")
         ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_ident (Ldot (Lident "M") "a"))) (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_field ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
     (Ldot (Ldot (Lident "A") "B") "x")))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "3" None)))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_constant ((pconst_desc (Pconst_integer "3" (Some 'l'))))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_constant ((pconst_desc (Pconst_integer "3" (Some 'L'))))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_constant ((pconst_desc (Pconst_integer "3" (Some 'n'))))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_char 'c')))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_string "abc" None)))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_let Nonrecursive
     (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ())))
     ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_let Recursive
     (((pvb_pat ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pvb_expr
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ())))
     ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_let Nonrecursive
     (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ()))
      ((pvb_pat ((ppat_desc (Ppat_var "y")) (ppat_attributes ()))) (pvb_expr
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ())))
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc (Pexp_ident (Lident "y"))) (pexp_attributes ()))))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_let Recursive
     (((pvb_pat ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pvb_expr
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ()))
      ((pvb_pat ((ppat_desc (Ppat_var "y")) (ppat_attributes ()))) (pvb_expr
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ())))
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc (Pexp_ident (Lident "y"))) (pexp_attributes ()))))))
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
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     ((pparam_desc
       (Pparam_val (Labelled "l") None
        ((ppat_desc (Ppat_var "l")) (ppat_attributes ())))))
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
       (Pparam_val (Labelled "l") None
        ((ppat_desc (Ppat_var "x2")) (ppat_attributes ())))))
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
       (Pparam_val (Optional "o") None
        ((ppat_desc (Ppat_var "o")) (ppat_attributes ())))))
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
       (Pparam_val (Optional "o")
        (Some
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
          (pexp_attributes ())))
        ((ppat_desc (Ppat_var "o")) (ppat_attributes ())))))
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
       (Pparam_val (Optional "o")
        (Some
         ((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
          (pexp_attributes ())))
        ((ppat_desc (Ppat_var "o2")) (ppat_attributes ())))))
     None
     (Pfunction_body
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function ((pparam_desc (Pparam_newtype "t"))) None
     (Pfunction_body
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     ((pparam_desc (Pparam_newtype "t"))
      (pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_var "a")) (ppat_attributes ())))))
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
     ((pparam_desc (Pparam_newtype "t")) (pparam_desc (Pparam_newtype "u")))
     None
     (Pfunction_body
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function () None
     (Pfunction_cases
      (((pc_lhs
        ((ppat_desc
         (Ppat_constant ((pconst_desc (Pconst_integer "0" None)))))
         (ppat_attributes ())))
        (pc_guard None) (pc_rhs
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ()))))
       ((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
        (pc_rhs
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
         (pexp_attributes ())))))
      ())))
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
      ((pexp_desc
       (Pexp_function () None
        (Pfunction_cases
         (((pc_lhs
           ((ppat_desc
            (Ppat_constant ((pconst_desc (Pconst_integer "0" None)))))
            (ppat_attributes ())))
           (pc_guard None) (pc_rhs
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
            (pexp_attributes ()))))
          ((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard
           None) (pc_rhs
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
            (pexp_attributes ())))))
         ())))
       (pexp_attributes ())))))
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
     ((Nolabel ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
      (Nolabel ((pexp_desc (Pexp_ident (Lident "y"))) (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
     (((Labelled "x")
       ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
      ((Optional "b")
       ((pexp_desc (Pexp_ident (Lident "b"))) (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
     (((Labelled "x")
       ((pexp_desc (Pexp_ident (Lident "x2"))) (pexp_attributes ())))
      ((Optional "b")
       ((pexp_desc (Pexp_ident (Lident "b"))) (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
     (((Labelled "x")
       ((pexp_desc
        (Pexp_apply
         ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
         ((Nolabel
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
            (pexp_attributes ())))
          (Nolabel
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
            (pexp_attributes ()))))))
        (pexp_attributes ())))
      ((Optional "b")
       ((pexp_desc (Pexp_construct (Lident "None") None)) (pexp_attributes
        ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_constant ((pconst_desc (Pconst_integer "1" None)))))
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
  (Pstr_eval
   ((pexp_desc
    (Pexp_try ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_construct (Lident "Not_found") None))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_try ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_construct (Lident "Not_found") None))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc
        (Ppat_construct (Lident "Failure")
         (Some (() ((ppat_desc (Ppat_var "msg")) (ppat_attributes ()))))))
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
        (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
       ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
       ()))
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_char 'x')))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ()))
      ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
       ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_construct (Lident "None") None)) (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_construct (Lident "Some")
     (Some
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_construct (Lident "More")
     (Some
      ((pexp_desc
       (Pexp_tuple
        (((pexp_desc
          (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
          (pexp_attributes ()))
         ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_char 'x')))))
          (pexp_attributes ()))
         ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes
          ())))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval ((pexp_desc (Pexp_variant "A" None)) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_variant "B"
     (Some
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "23" None)))))
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
        (pexp_attributes ()))))
     (Some ((pexp_desc (Pexp_ident (Lident "r"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     (((Lident "x")
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))
     (Some
      ((pexp_desc
       (Pexp_apply
        ((pexp_desc (Pexp_ident (Lident "f"))) (pexp_attributes ()))
        ((Nolabel
          ((pexp_desc
           (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
           (pexp_attributes ()))))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_field ((pexp_desc (Pexp_ident (Lident "r"))) (pexp_attributes ()))
     (Lident "a")))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_field ((pexp_desc (Pexp_ident (Lident "r"))) (pexp_attributes ()))
     (Ldot (Lident "X") "a")))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_setfield
     ((pexp_desc (Pexp_ident (Lident "r"))) (pexp_attributes ()))
     (Lident "a")
     ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_setfield
     ((pexp_desc (Pexp_ident (Lident "r"))) (pexp_attributes ()))
     (Ldot (Lident "X") "a")
     ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval ((pexp_desc (Pexp_array ())) (pexp_attributes ())) ()))
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
     ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ()))
     (Some
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_ifthenelse
     ((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
     ((pexp_desc
      (Pexp_ifthenelse
       ((pexp_desc (Pexp_ident (Lident "b"))) (pexp_attributes ()))
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))
       (Some
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
         (pexp_attributes ())))))
      (pexp_attributes ()))
     (Some
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "3" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
      ((pexp_desc (Pexp_ident (Lident "b"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc (Pexp_ident (Lident "a"))) (pexp_attributes ()))
      ((pexp_desc (Pexp_ident (Lident "b"))) (pexp_attributes ()))
      ((pexp_desc (Pexp_ident (Lident "c"))) (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_while
     ((pexp_desc (Pexp_construct (Lident "true") None)) (pexp_attributes ()))
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_for ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "0" None)))))
      (pexp_attributes ()))
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "9" None)))))
      (pexp_attributes ()))
     Upto
     ((pexp_desc
      (Pexp_array
       (((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
         (pexp_attributes ())))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_constraint
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ()))
     ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_coerce ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     None ((ptyp_desc (Ptyp_constr (Lident "t2") ())) (ptyp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_coerce ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (Some ((ptyp_desc (Ptyp_constr (Lident "t1") ())) (ptyp_attributes ())))
     ((ptyp_desc (Ptyp_constr (Lident "t2") ())) (ptyp_attributes ()))))
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
  (Pstr_eval ((pexp_desc (Pexp_new (Lident "a"))) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_new (Ldot (Lident "M") "a"))) (pexp_attributes ())) ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_setinstvar "x"
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ()))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_override
     (("x"
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
      ("y"
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
        (pexp_attributes ()))))))
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
      (((pcf_desc
        (Pcf_method "x" Public
         (Cfk_concrete Fresh
          ((pexp_desc
           (Pexp_poly
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
             (pexp_attributes ()))
            None))
           (pexp_attributes ())))))
        (pcf_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_object
     ((pcstr_self ((ppat_desc Ppat_any) (ppat_attributes ()))) (pcstr_fields
      (((pcf_desc
        (Pcf_method "x" Public
         (Cfk_concrete Fresh
          ((pexp_desc
           (Pexp_poly
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
             (pexp_attributes ()))
            (Some
             ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
              ())))))
           (pexp_attributes ())))))
        (pcf_attributes ())))))))
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
        (Pcf_method "x" Public
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
        (Pcf_method "y" Public
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
    (Pexp_object
     ((pcstr_self ((ppat_desc (Ppat_var "myself")) (ppat_attributes ())))
      (pcstr_fields
      (((pcf_desc
        (Pcf_method "x" Public
         (Cfk_concrete Fresh
          ((pexp_desc
           (Pexp_poly
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
             (pexp_attributes ()))
            None))
           (pexp_attributes ())))))
        (pcf_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_object
     ((pcstr_self
      ((ppat_desc
       (Ppat_alias ((ppat_desc (Ppat_var "a")) (ppat_attributes ()))
        "myself"))
       (ppat_attributes ())))
      (pcstr_fields
      (((pcf_desc
        (Pcf_method "x" Public
         (Cfk_concrete Fresh
          ((pexp_desc
           (Pexp_poly
            ((pexp_desc
             (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
             (pexp_attributes ()))
            None))
           (pexp_attributes ())))))
        (pcf_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function ((pparam_desc (Pparam_newtype "x"))) None
     (Pfunction_body
      ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
       (pexp_attributes ())))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_extension ("ext1" (PStr ())))) (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
     (((pc_lhs ((ppat_desc Ppat_any) (ppat_attributes ()))) (pc_guard None)
       (pc_rhs ((pexp_desc Pexp_unreachable) (pexp_attributes ())))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_type Nonrecursive
   (((ptype_name "ptype_abstract_1") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind Ptype_abstract) (ptype_private Public) (ptype_manifest None)
     (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "ptype_abstract_2") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind Ptype_abstract) (ptype_private Public) (ptype_manifest
     (Some
      ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
     (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "ptype_abstract_3") (ptype_params
     ((((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ()))
       (NoVariance NoInjectivity))))
     (ptype_cstrs ()) (ptype_kind Ptype_abstract) (ptype_private Public)
     (ptype_manifest
     (Some
      ((ptyp_desc
       (Ptyp_constr (Lident "option")
        (((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ())))))
       (ptyp_attributes ()))))
     (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "ptype_variant_1") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_variant
      (((pcd_name "A") (pcd_vars ()) (pcd_args (Pcstr_tuple ())) (pcd_res
        None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "ptype_variant_2") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_variant
      (((pcd_name "B") (pcd_vars ()) (pcd_args (Pcstr_tuple ())) (pcd_res
        None) (pcd_attributes ()))
       ((pcd_name "C") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ()))
       ((pcd_name "D") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "ptype_record_1") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_record
      (((pld_name "a") (pld_mutable Immutable) (pld_type
        ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))
        (pld_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "ptype_record_2") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_record
      (((pld_name "b") (pld_mutable Immutable) (pld_type
        ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))
        (pld_attributes ()))
       ((pld_name "c") (pld_mutable Mutable) (pld_type
        ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes ())))
        (pld_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "ptype_open_1") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind Ptype_open) (ptype_private Public) (ptype_manifest None)
     (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "pcstr_tuple_1") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_variant
      (((pcd_name "A2") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "pcstr_tuple_2") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_variant
      (((pcd_name "B2") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
          ((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "pcstr_record_1") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_variant
      (((pcd_name "A3") (pcd_vars ()) (pcd_args
        (Pcstr_record
         (((pld_name "a") (pld_mutable Immutable) (pld_type
           ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))
           (pld_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "pcstr_record_2") (ptype_params ()) (ptype_cstrs ())
     (ptype_kind
     (Ptype_variant
      (((pcd_name "B3") (pcd_vars ()) (pcd_args
        (Pcstr_record
         (((pld_name "a") (pld_mutable Immutable) (pld_type
           ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))
           (pld_attributes ()))
          ((pld_name "b") (pld_mutable Mutable) (pld_type
           ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes
            ())))
           (pld_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_typext
   ((ptyext_path (Lident "ptype_open_1")) (ptyext_params ())
    (ptyext_constructors
    (((pext_name "Pext_decl_1") (pext_kind
      (Pext_decl () (Pcstr_tuple ()) None)) (pext_attributes ()))))
    (ptyext_private Public) (ptyext_attributes ()))))
 (pstr_desc
  (Pstr_typext
   ((ptyext_path (Lident "ptype_open_1")) (ptyext_params ())
    (ptyext_constructors
    (((pext_name "Pext_decl_2") (pext_kind
      (Pext_decl ()
       (Pcstr_tuple
        (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
       None))
      (pext_attributes ()))))
    (ptyext_private Public) (ptyext_attributes ()))))
 (pstr_desc
  (Pstr_typext
   ((ptyext_path (Lident "ptype_open_1")) (ptyext_params ())
    (ptyext_constructors
    (((pext_name "Pext_decl_3") (pext_kind
      (Pext_decl ()
       (Pcstr_record
        (((pld_name "a") (pld_mutable Immutable) (pld_type
          ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))
          (pld_attributes ()))))
       None))
      (pext_attributes ()))))
    (ptyext_private Public) (ptyext_attributes ()))))
 (pstr_desc
  (Pstr_typext
   ((ptyext_path (Lident "ptype_open_1")) (ptyext_params ())
    (ptyext_constructors
    (((pext_name "Pext_rebind_1") (pext_kind
      (Pext_rebind (Lident "Pext_decl_1"))) (pext_attributes ()))))
    (ptyext_private Public) (ptyext_attributes ()))))
 (pstr_desc
  (Pstr_exception
   ((ptyexn_constructor
    ((pext_name "Pext_rebind_2") (pext_kind
     (Pext_rebind (Lident "Not_found"))) (pext_attributes ())))
    (ptyexn_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_ident_1") (pmtd_type
    (Some ((pmty_desc (Pmty_ident (Lident "S"))) (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_ident_2") (pmtd_type
    (Some
     ((pmty_desc (Pmty_ident (Ldot (Lident "M") "S"))) (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_signature_1") (pmtd_type
    (Some ((pmty_desc (Pmty_signature ())) (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_signature_2") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_value
          ((pval_name "x") (pval_type
           ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))
           (pval_prim ()) (pval_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_typeof_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_typeof
       ((pmod_desc (Pmod_ident (Lident "M"))) (pmod_attributes ()))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_typeof_2") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_typeof ((pmod_desc (Pmod_structure ())) (pmod_attributes ()))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_extension_1") (pmtd_type
    (Some
     ((pmty_desc (Pmty_extension ("ext" (PStr ())))) (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Pmty_alias_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_module
          ((pmd_name (Some "X")) (pmd_type
           ((pmty_desc (Pmty_alias (Lident "M"))) (pmty_attributes ())))
           (pmd_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_value_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_value
          ((pval_name "x") (pval_type
           ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))
           (pval_prim ()) (pval_attributes ()))))
        (psig_desc
         (Psig_value
          ((pval_name "f") (pval_type
           ((ptyp_desc
            (Ptyp_arrow Nolabel
             ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
              ()))
             ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
              ()))))
            (ptyp_attributes ())))
           (pval_prim ("f_stub")) (pval_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_type_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_type Nonrecursive
          (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
            Ptype_abstract) (ptype_private Public) (ptype_manifest None)
            (ptype_attributes ())))))
        (psig_desc
         (Psig_type Nonrecursive
          (((ptype_name "u") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
            Ptype_abstract) (ptype_private Public) (ptype_manifest
            (Some
             ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
              ()))))
            (ptype_attributes ())))))
        (psig_desc
         (Psig_type Recursive
          (((ptype_name "v") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
            (Ptype_variant
             (((pcd_name "A") (pcd_vars ()) (pcd_args
               (Pcstr_tuple
                (((ptyp_desc (Ptyp_constr (Lident "int") ()))
                  (ptyp_attributes ())))))
               (pcd_res None) (pcd_attributes ())))))
            (ptype_private Public) (ptype_manifest None) (ptype_attributes
            ()))
           ((ptype_name "w") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
            (Ptype_record
             (((pld_name "a") (pld_mutable Immutable) (pld_type
               ((ptyp_desc (Ptyp_constr (Lident "v") ())) (ptyp_attributes
                ())))
               (pld_attributes ())))))
            (ptype_private Public) (ptype_manifest None) (ptype_attributes
            ()))))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_typext_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_type Recursive
          (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
            Ptype_open) (ptype_private Public) (ptype_manifest None)
            (ptype_attributes ())))))
        (psig_desc
         (Psig_typext
          ((ptyext_path (Lident "t")) (ptyext_params ()) (ptyext_constructors
           (((pext_name "A") (pext_kind
             (Pext_decl ()
              (Pcstr_tuple
               (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
                 ()))))
              None))
             (pext_attributes ()))))
           (ptyext_private Public) (ptyext_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_exception_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_exception
          ((ptyexn_constructor
           ((pext_name "E") (pext_kind
            (Pext_decl ()
             (Pcstr_tuple
              (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes
                ()))))
             None))
            (pext_attributes ())))
           (ptyexn_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_module_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_module
          ((pmd_name (Some "X")) (pmd_type
           ((pmty_desc (Pmty_signature ())) (pmty_attributes ())))
           (pmd_attributes ()))))
        (psig_desc
         (Psig_module
          ((pmd_name (Some "Y")) (pmd_type
           ((pmty_desc (Pmty_alias (Lident "M"))) (pmty_attributes ())))
           (pmd_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_modtype_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_modtype
          ((pmtd_name "T") (pmtd_type None) (pmtd_attributes ()))))
        (psig_desc
         (Psig_modtype
          ((pmtd_name "U") (pmtd_type
           (Some ((pmty_desc (Pmty_signature ())) (pmty_attributes ()))))
           (pmtd_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_open_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_open
          ((popen_expr (Lident "M")) (popen_override Fresh) (popen_attributes
           ()))))
        (psig_desc
         (Psig_open
          ((popen_expr (Ldot (Lident "M") "N")) (popen_override Override)
           (popen_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_include_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_include
          ((pincl_mod
           ((pmty_desc (Pmty_ident (Lident "S"))) (pmty_attributes ())))
           (pincl_attributes ()))))
        (psig_desc
         (Psig_include
          ((pincl_mod
           ((pmty_desc
            (Pmty_typeof
             ((pmod_desc (Pmod_ident (Lident "M"))) (pmod_attributes ()))))
            (pmty_attributes ())))
           (pincl_attributes ())))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_attribute_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature
       ((psig_desc
         (Psig_attribute ((attr_name "attr") (attr_payload (PStr ()))))))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Psig_extension_1") (pmtd_type
    (Some
     ((pmty_desc
      (Pmty_signature ((psig_desc (Psig_extension ("ext" (PStr ())) ())))))
      (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_ident_1")) (pmb_expr
    ((pmod_desc (Pmod_ident (Lident "X"))) (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_ident_2")) (pmb_expr
    ((pmod_desc (Pmod_ident (Ldot (Lident "X") "Y"))) (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_structure_1")) (pmb_expr
    ((pmod_desc (Pmod_structure ())) (pmod_attributes ()))) (pmb_attributes
    ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_structure_2")) (pmb_expr
    ((pmod_desc
     (Pmod_structure
      ((pstr_desc
        (Pstr_value Nonrecursive
         (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ())))
           (pvb_expr
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
            (pexp_attributes ())))
           (pvb_constraint None) (pvb_attributes ()))))))))
     (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_apply_1")) (pmb_expr
    ((pmod_desc
     (Pmod_apply ((pmod_desc (Pmod_ident (Lident "F"))) (pmod_attributes ()))
      ((pmod_desc (Pmod_ident (Lident "X"))) (pmod_attributes ()))))
     (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_apply_2")) (pmb_expr
    ((pmod_desc
     (Pmod_apply
      ((pmod_desc
       (Pmod_apply
        ((pmod_desc (Pmod_ident (Lident "F"))) (pmod_attributes ()))
        ((pmod_desc (Pmod_ident (Lident "X"))) (pmod_attributes ()))))
       (pmod_attributes ()))
      ((pmod_desc (Pmod_ident (Lident "Y"))) (pmod_attributes ()))))
     (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_apply_unit_1")) (pmb_expr
    ((pmod_desc
     (Pmod_apply_unit
      ((pmod_desc (Pmod_ident (Lident "F"))) (pmod_attributes ()))))
     (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_constraint_1")) (pmb_expr
    ((pmod_desc
     (Pmod_constraint
      ((pmod_desc (Pmod_ident (Lident "X"))) (pmod_attributes ()))
      ((pmty_desc (Pmty_ident (Lident "S"))) (pmty_attributes ()))))
     (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_constraint_2")) (pmb_expr
    ((pmod_desc
     (Pmod_constraint ((pmod_desc (Pmod_structure ())) (pmod_attributes ()))
      ((pmty_desc (Pmty_ident (Lident "S"))) (pmty_attributes ()))))
     (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_unpack_1")) (pmb_expr
    ((pmod_desc
     (Pmod_unpack
      ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))))
     (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pmod_extension_1")) (pmb_expr
    ((pmod_desc (Pmod_extension ("ext" (PStr ())))) (pmod_attributes ())))
    (pmb_attributes ()))))
 (pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply ((pexp_desc (Pexp_ident (Lident "+"))) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ())))
      (Nolabel
       ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
        (pexp_attributes ()))))))
    (pexp_attributes ()))
   ()))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Recursive
   (((pvb_pat ((ppat_desc (Ppat_var "x")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc (Ppat_construct (Lident "()") None)) (ppat_attributes
           ())))))
       None
       (Pfunction_body
        ((pexp_desc
         (Pexp_apply
          ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ()))
          ((Nolabel
            ((pexp_desc (Pexp_construct (Lident "()") None)) (pexp_attributes
             ()))))))
         (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e1")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ()))
    ((pvb_pat ((ppat_desc (Ppat_var "e2")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "2" None)))))
      (pexp_attributes ())))
     (pvb_constraint None) (pvb_attributes ())))))
 (pstr_desc
  (Pstr_primitive
   ((pval_name "x") (pval_type
    ((ptyp_desc
     (Ptyp_arrow Nolabel
      ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
      ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
     (ptyp_attributes ())))
    (pval_prim ("prim_stub")) (pval_attributes ()))))
 (pstr_desc
  (Pstr_primitive
   ((pval_name "x") (pval_type
    ((ptyp_desc
     (Ptyp_arrow Nolabel
      ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
      ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
     (ptyp_attributes ())))
    (pval_prim ("prim_stub" "prim_stub_native")) (pval_attributes ()))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     Ptype_abstract) (ptype_private Public) (ptype_manifest
     (Some
      ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
     (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Nonrecursive
   (((ptype_name "t") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     Ptype_abstract) (ptype_private Public) (ptype_manifest
     (Some
      ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))))
     (ptype_attributes ())))))
 (pstr_desc
  (Pstr_type Recursive
   (((ptype_name "t1") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "A4") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ()))
    ((ptype_name "t2") (ptype_params ()) (ptype_cstrs ()) (ptype_kind
     (Ptype_variant
      (((pcd_name "B4") (pcd_vars ()) (pcd_args
        (Pcstr_tuple
         (((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ())))))
        (pcd_res None) (pcd_attributes ())))))
     (ptype_private Public) (ptype_manifest None) (ptype_attributes ())))))
 (pstr_desc
  (Pstr_typext
   ((ptyext_path (Lident "t")) (ptyext_params ()) (ptyext_constructors
    (((pext_name "Pstr_typext_1") (pext_kind
      (Pext_decl ()
       (Pcstr_tuple
        (((ptyp_desc (Ptyp_constr (Lident "bool") ())) (ptyp_attributes ()))))
       None))
      (pext_attributes ()))))
    (ptyext_private Public) (ptyext_attributes ()))))
 (pstr_desc
  (Pstr_exception
   ((ptyexn_constructor
    ((pext_name "Pstr_exception_1") (pext_kind
     (Pext_decl () (Pcstr_tuple ()) None)) (pext_attributes ())))
    (ptyexn_attributes ()))))
 (pstr_desc
  (Pstr_exception
   ((ptyexn_constructor
    ((pext_name "Pstr_exception_2") (pext_kind
     (Pext_decl ()
      (Pcstr_tuple
       (((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ()))
        ((ptyp_desc (Ptyp_constr (Lident "string") ())) (ptyp_attributes ()))))
      None))
     (pext_attributes ())))
    (ptyexn_attributes ()))))
 (pstr_desc
  (Pstr_module
   ((pmb_name (Some "Pstr_module_1")) (pmb_expr
    ((pmod_desc (Pmod_structure ())) (pmod_attributes ()))) (pmb_attributes
    ()))))
 (pstr_desc
  (Pstr_modtype
   ((pmtd_name "Mt") (pmtd_type
    (Some ((pmty_desc (Pmty_signature ())) (pmty_attributes ()))))
    (pmtd_attributes ()))))
 (pstr_desc
  (Pstr_open
   ((popen_expr ((pmod_desc (Pmod_ident (Lident "M"))) (pmod_attributes ())))
    (popen_override Fresh) (popen_attributes ()))))
 (pstr_desc
  (Pstr_open
   ((popen_expr
    ((pmod_desc (Pmod_ident (Ldot (Lident "M") "N"))) (pmod_attributes ())))
    (popen_override Override) (popen_attributes ()))))
 (pstr_desc
  (Pstr_open
   ((popen_expr
    ((pmod_desc
     (Pmod_structure
      ((pstr_desc
        (Pstr_value Nonrecursive
         (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ())))
           (pvb_expr
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
            (pexp_attributes ())))
           (pvb_constraint None) (pvb_attributes ()))))))))
     (pmod_attributes ())))
    (popen_override Fresh) (popen_attributes ()))))
 (pstr_desc
  (Pstr_include
   ((pincl_mod ((pmod_desc (Pmod_ident (Lident "M"))) (pmod_attributes ())))
    (pincl_attributes ()))))
 (pstr_desc
  (Pstr_include
   ((pincl_mod
    ((pmod_desc
     (Pmod_structure
      ((pstr_desc
        (Pstr_value Nonrecursive
         (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ())))
           (pvb_expr
           ((pexp_desc
            (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
            (pexp_attributes ())))
           (pvb_constraint None) (pvb_attributes ()))))))))
     (pmod_attributes ())))
    (pincl_attributes ()))))
 (pstr_desc (Pstr_attribute ((attr_name "attr") (attr_payload (PStr ())))))
 (pstr_desc
  (Pstr_attribute
   ((attr_name "attr") (attr_payload
    (PStr
     ((pstr_desc
       (Pstr_eval
        ((pexp_desc
         (Pexp_constant ((pconst_desc (Pconst_string "payload" None)))))
         (pexp_attributes ()))
        ()))))))))
 (pstr_desc (Pstr_extension ("ext" (PStr ())) ()))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_constant ((pconst_desc (Pconst_integer "1" None)))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc (Ppat_var "x")) (ppat_attributes ())))))
       None
       (Pfunction_body
        ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ()) (typ
       ((ptyp_desc
        (Ptyp_poly ("a")
         ((ptyp_desc
          (Ptyp_arrow Nolabel
           ((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ()))
           ((ptyp_desc (Ptyp_var "a")) (ptyp_attributes ()))))
          (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc
      (Pexp_function
       ((pparam_desc
         (Pparam_val Nolabel None
          ((ppat_desc (Ppat_var "x")) (ppat_attributes ())))))
       None
       (Pfunction_body
        ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))))
      (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_constraint ((locally_abstract_univars ("a")) (typ
       ((ptyp_desc
        (Ptyp_arrow Nolabel
         ((ptyp_desc (Ptyp_constr (Lident "a") ())) (ptyp_attributes ()))
         ((ptyp_desc (Ptyp_constr (Lident "a") ())) (ptyp_attributes ()))))
        (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_coercion ((ground None) (coercion
       ((ptyp_desc (Ptyp_constr (Lident "int") ())) (ptyp_attributes ())))))))
     (pvb_attributes ())))))
 (pstr_desc
  (Pstr_value Nonrecursive
   (((pvb_pat ((ppat_desc (Ppat_var "e")) (ppat_attributes ()))) (pvb_expr
     ((pexp_desc (Pexp_ident (Lident "x"))) (pexp_attributes ())))
     (pvb_constraint
     (Some
      (Pvc_coercion ((ground
       (Some
        ((ptyp_desc (Ptyp_constr (Lident "t1") ())) (ptyp_attributes ()))))
       (coercion
       ((ptyp_desc (Ptyp_constr (Lident "t2") ())) (ptyp_attributes ())))))))
     (pvb_attributes ()))))))
