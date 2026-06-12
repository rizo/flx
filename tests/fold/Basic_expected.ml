(((pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1))))) (pexp_loc
    (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_ident ((txt (Ldot (Lident "X") "a")) (loc (* -1 -1)))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_ident ((txt (Ldot (Ldot (Lident "X1") "X2") "a")) (loc (* -1 -1)))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_constant
     ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_constant ((pconst_desc (Pconst_char 'x')) (pconst_loc (* -1 -1)))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_constant
     ((pconst_desc (Pconst_string "hello" (* -1 -1) None)) (pconst_loc
      (* -1 -1)))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_let Nonrecursive
     (((pvb_pat
       ((ppat_desc (Ppat_var ((txt "a") (loc (* -1 -1))))) (ppat_loc
        (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pvb_expr
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
       (pvb_constraint None) (pvb_attributes ()) (pvb_loc (* -1 -1))))
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident ((txt (Lident "+")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     (((pparam_loc (* -1 -1)) (pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_var ((txt "x") (loc (* -1 -1))))) (ppat_loc
         (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ()))))))
     None
     (Pfunction_body
      ((pexp_desc (Pexp_ident ((txt (Lident "x")) (loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_function
     (((pparam_loc (* -1 -1)) (pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_var ((txt "x") (loc (* -1 -1))))) (ppat_loc
         (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))))
      ((pparam_loc (* -1 -1)) (pparam_desc
       (Pparam_val Nolabel None
        ((ppat_desc (Ppat_var ((txt "y") (loc (* -1 -1))))) (ppat_loc
         (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ()))))))
     None
     (Pfunction_body
      ((pexp_desc
       (Pexp_apply
        ((pexp_desc (Pexp_ident ((txt (Lident "+")) (loc (* -1 -1)))))
         (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
        ((Nolabel
          ((pexp_desc (Pexp_ident ((txt (Lident "x")) (loc (* -1 -1)))))
           (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
         (Nolabel
          ((pexp_desc (Pexp_ident ((txt (Lident "y")) (loc (* -1 -1)))))
           (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply
     ((pexp_desc (Pexp_ident ((txt (Lident "f")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc (Pexp_ident ((txt (Lident "x")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply
     ((pexp_desc (Pexp_ident ((txt (Lident "f")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc (Pexp_ident ((txt (Lident "x1")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc (Pexp_ident ((txt (Lident "x2")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply
     ((pexp_desc (Pexp_ident ((txt (Lident "f")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((Labelled "x1")
       ((pexp_desc (Pexp_ident ((txt (Lident "x1")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc (Pexp_ident ((txt (Lident "x2")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply
     ((pexp_desc (Pexp_ident ((txt (Lident "f")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((Labelled "x1")
       ((pexp_desc (Pexp_ident ((txt (Lident "x1")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      ((Labelled "x2")
       ((pexp_desc (Pexp_ident ((txt (Lident "x2")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc
        (Pexp_construct ((txt (Lident "()")) (loc (* -1 -1))) None))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply
     ((pexp_desc (Pexp_ident ((txt (Lident "~-")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply
     ((pexp_desc (Pexp_ident ((txt (Lident "+")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_apply
     ((pexp_desc (Pexp_ident ((txt (Lident "+")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((Nolabel
       ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      (Nolabel
       ((pexp_desc
        (Pexp_apply
         ((pexp_desc (Pexp_ident ((txt (Lident "~-")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
         ((Nolabel
           ((pexp_desc
            (Pexp_constant
             ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
            (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match
     ((pexp_desc (Pexp_ident ((txt (Lident "e")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc (Ppat_var ((txt "x") (loc (* -1 -1))))) (ppat_loc
        (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc (Pexp_ident ((txt (Lident "x")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match
     ((pexp_desc (Pexp_ident ((txt (Lident "e")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc (Ppat_var ((txt "x") (loc (* -1 -1))))) (ppat_loc
        (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match
     ((pexp_desc (Pexp_ident ((txt (Lident "e")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc
        (Ppat_alias
         ((ppat_desc
          (Ppat_constant
           ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
          (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ()))
         ((txt "x") (loc (* -1 -1)))))
        (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match
     ((pexp_desc (Pexp_ident ((txt (Lident "e")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_or
         ((ppat_desc
          (Ppat_constant
           ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
          (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ()))
         ((ppat_desc
          (Ppat_constant
           ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
          (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ()))))
        (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc (Ppat_var ((txt "x") (loc (* -1 -1))))) (ppat_loc
        (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident ((txt (Lident "+")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc
        (Ppat_or
         ((ppat_desc
          (Ppat_constant
           ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
          (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ()))
         ((ppat_desc
          (Ppat_constant
           ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
          (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ()))))
        (ppat_loc (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
      ((pc_lhs
       ((ppat_desc (Ppat_var ((txt "x") (loc (* -1 -1))))) (ppat_loc
        (* -1 -1)) (ppat_loc_stack ()) (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc (Pexp_ident ((txt (Lident "b")) (loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "3" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "3" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "4" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "5" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "6" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "7" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "8" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "9" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "10" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_tuple
     (((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_tuple
        (((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
         ((pexp_desc (Pexp_ident ((txt (Lident "b")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_construct ((txt (Lident "[]")) (loc (* -1 -1))) None))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_construct ((txt (Lident "true")) (loc (* -1 -1))) None))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_construct ((txt (Lident "false")) (loc (* -1 -1))) None)) (pexp_loc
    (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_variant "Green" None)) (pexp_loc (* -1 -1))
    (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_variant "Rgb"
     (Some
      ((pexp_desc
       (Pexp_tuple
        (((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "255" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
         ((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
         ((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     ((((txt (Lident "x")) (loc (* -1 -1)))
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
     None))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     ((((txt (Lident "x")) (loc (* -1 -1)))
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      (((txt (Lident "y")) (loc (* -1 -1)))
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
     None))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     ((((txt (Lident "x")) (loc (* -1 -1)))
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
      (((txt (Lident "z")) (loc (* -1 -1)))
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
     (Some
      ((pexp_desc (Pexp_ident ((txt (Lident "p")) (loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_record
     ((((txt (Lident "x")) (loc (* -1 -1)))
       ((pexp_desc
        (Pexp_constant
         ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
     (Some
      ((pexp_desc (Pexp_ident ((txt (Lident "p")) (loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc (Pexp_array ())) (pexp_loc (* -1 -1)) (pexp_loc_stack ())
    (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "3" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_ifthenelse
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident ((txt (Lident ">")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "0" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((pexp_desc (Pexp_ident ((txt (Lident "e1")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (Some
      ((pexp_desc (Pexp_ident ((txt (Lident "e2")) (loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_array
     (((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
      ((pexp_desc
       (Pexp_constant
        ((pconst_desc (Pconst_integer "3" None)) (pconst_loc (* -1 -1)))))
       (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_while
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident ((txt (Lident ">")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident ((txt (Lident "print")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_for
     ((ppat_desc (Ppat_var ((txt "i") (loc (* -1 -1))))) (ppat_loc (* -1 -1))
      (ppat_loc_stack ()) (ppat_attributes ()))
     ((pexp_desc (Pexp_ident ((txt (Lident "e1")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((pexp_desc (Pexp_ident ((txt (Lident "e2")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     Upto
     ((pexp_desc
      (Pexp_array
       (((pexp_desc (Pexp_ident ((txt (Lident "e3")) (loc (* -1 -1)))))
         (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_for
     ((ppat_desc (Ppat_var ((txt "i") (loc (* -1 -1))))) (ppat_loc (* -1 -1))
      (ppat_loc_stack ()) (ppat_attributes ()))
     ((pexp_desc (Pexp_ident ((txt (Lident "e1")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((pexp_desc (Pexp_ident ((txt (Lident "e2")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     Downto
     ((pexp_desc (Pexp_ident ((txt (Lident "e3")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_for
     ((ppat_desc (Ppat_var ((txt "i") (loc (* -1 -1))))) (ppat_loc (* -1 -1))
      (ppat_loc_stack ()) (ppat_attributes ()))
     ((pexp_desc (Pexp_ident ((txt (Lident "e1")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     ((pexp_desc (Pexp_ident ((txt (Lident "e2")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     Upto
     ((pexp_desc
      (Pexp_array
       (((pexp_desc
         (Pexp_apply
          ((pexp_desc (Pexp_ident ((txt (Lident "print")) (loc (* -1 -1)))))
           (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
          ((Nolabel
            ((pexp_desc
             (Pexp_constant
              ((pconst_desc (Pconst_string "hello" (* -1 -1) None))
               (pconst_loc (* -1 -1)))))
             (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
         (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
        ((pexp_desc
         (Pexp_apply
          ((pexp_desc (Pexp_ident ((txt (Lident "f")) (loc (* -1 -1)))))
           (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
          ((Nolabel
            ((pexp_desc
             (Pexp_construct ((txt (Lident "()")) (loc (* -1 -1))) None))
             (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
         (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_assert
     ((pexp_desc (Pexp_ident ((txt (Lident "true")) (loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_assert
     ((pexp_desc
      (Pexp_apply
       ((pexp_desc (Pexp_ident ((txt (Lident ">")) (loc (* -1 -1)))))
        (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
       ((Nolabel
         ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ())))
        (Nolabel
         ((pexp_desc
          (Pexp_constant
           ((pconst_desc (Pconst_integer "2" None)) (pconst_loc (* -1 -1)))))
          (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_lazy
     ((pexp_desc
      (Pexp_constant
       ((pconst_desc (Pconst_integer "1" None)) (pconst_loc (* -1 -1)))))
      (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc Pexp_unreachable) (pexp_loc (* -1 -1)) (pexp_loc_stack ())
    (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1)))
 ((pstr_desc
  (Pstr_eval
   ((pexp_desc
    (Pexp_match
     ((pexp_desc (Pexp_ident ((txt (Lident "a")) (loc (* -1 -1))))) (pexp_loc
      (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
     (((pc_lhs
       ((ppat_desc Ppat_any) (ppat_loc (* -1 -1)) (ppat_loc_stack ())
        (ppat_attributes ())))
       (pc_guard None) (pc_rhs
       ((pexp_desc Pexp_unreachable) (pexp_loc (* -1 -1)) (pexp_loc_stack ())
        (pexp_attributes ())))))))
    (pexp_loc (* -1 -1)) (pexp_loc_stack ()) (pexp_attributes ()))
   ()))
  (pstr_loc (* -1 -1))))
