open Kawa

exception Error of string

let error s = raise (Error s)

let type_error ?(context="") ty_actual ty_expected =
  let context_msg = if context = "" then "" else " in " ^ context in
  error
    (Printf.sprintf "Error%s: expected %s, got %s"
       context_msg
       (typ_to_string ty_expected)
       (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv = List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let type_of_unop = function Opp -> TInt | Not -> TBool 
  | TypeCast (newType) -> newType

let type_of_binop = function
  | Add | Sub | Mul | Div | Rem -> TInt
  | Lt | Le | Gt | Ge | Eq | Neq | And | Or -> TBool

 
let check_eq_type ?(context="") expected actual =
  if expected <> actual then type_error ~context actual expected

let rec check_subtype objective curr (find_class_def: string -> class_def) =
  if objective = curr then ()
  else match curr with
  | TClass name -> 
      let classdef = find_class_def name in
      begin match classdef.parent with
      | Some parentname -> 
          let parentclsdef = find_class_def parentname in
          check_subtype objective (TClass parentclsdef.class_name) find_class_def
      | None -> error ("No parent class found for " ^ name)
      end
  | _ -> error ("Cannot check subtype for primitive type: " ^ typ_to_string curr)

let find_class_def class_name classes =
  try List.find (fun cls -> cls.class_name = class_name) classes
  with Not_found -> error ("Class not found: " ^ class_name)

let find_method_def meth_name methods =
  match List.find_opt (fun m -> m.method_name = meth_name) methods with
  | Some m -> m
  | None -> error ("Method not found: " ^ meth_name)

let objname_of_typ = function TClass clsname -> clsname | _ -> assert false

let typecheck_prog (p:program) : program =
  let tenv = Env.empty in
  let tenv = add_env p.globals tenv in
 
  let find_class_def class_name = find_class_def class_name p.classes in
  let check_subtype objective curr = check_subtype objective curr find_class_def in

  let rec check_expr (e:expr) (tenv: tenv) : expr =
    match e.expr with
    | Int _ -> e
    | Bool _ -> e
    | Unop (u, e) -> (
        let typed_e = check_expr e tenv in
        match u with
        | Opp ->
            check_eq_type TInt typed_e.annot;
            e
        | Not ->
            check_eq_type TBool typed_e.annot;
            e
        | TypeCast (newType) -> let typed_e = check_expr e tenv in 
            (try 
              check_subtype newType typed_e.annot ; {annot = newType ; expr = typed_e.expr}
            with
            | _ ->  check_subtype typed_e.annot newType ;{annot = newType ; expr = typed_e.expr})
            )

    | Binop (u, e1, e2) -> (
        let typed_e1 = check_expr e1 tenv in
        let typed_e2 = check_expr e2 tenv in
        match u with
        | Eq -> check_eq_type typed_e1.annot typed_e2.annot; {annot = TBool ; expr = e.expr}
        | Lt | Le | Gt | Ge | Neq ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            {annot = TBool ; expr = e.expr}
        | Add | Sub | Mul | Div | Rem ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            {annot = TInt ; expr = e.expr}
        | And | Or ->
            check_eq_type TBool typed_e1.annot;
            check_eq_type TBool typed_e2.annot;
            {annot = TBool ; expr = e.expr})
    | Get m -> {annot =  type_mem_access m tenv ; expr = e.expr}
    | This -> begin 
        try 
        (* let c = List.find (fun cls -> cls.class_name = "this") tenv *)
        let c = Env.find "this" tenv 
        in 
        (* TClass (c.class_name) *)
        {annot = c ; expr = e.expr}
        with Not_found -> error ("Class not found: " ^ "this")
    end

    | New class_name -> 
      (* todo check si la classe existe *)
      {annot = TClass class_name ; expr = e.expr}
    | NewCstr (class_name, args) ->
        let defclass = find_class_def class_name in
        let constructor = find_method_def "constructor" defclass.methods in
        let param_types = List.map snd constructor.params in
        let arg_types = List.map (fun arg -> (check_expr arg tenv).annot) args in
        List.iter2 check_subtype param_types arg_types;
        {annot = TClass class_name ; expr = e.expr}

    | MethCall (obj, meth_name, args) ->
        let typcls = objname_of_typ ((check_expr obj tenv).annot) in
        let defclass = find_class_def typcls in
        let methodeu = find_method_def meth_name defclass.methods in
        let param_types = List.map snd methodeu.params in
        let arg_types = List.map (fun arg -> (check_expr arg tenv).annot) args in
        List.iter2 check_subtype param_types arg_types;
        {annot = methodeu.return ; expr = e.expr}

  and type_mem_access m tenv : typ =
    match m with
    | Var name -> (
        match Env.find_opt name tenv with
        | Some typ -> typ
        | None -> error ("Undeclared variable: " ^ name))
    | Field (obj, field_name) -> (
        let cls_name = objname_of_typ ((check_expr obj tenv).annot) in
        let class_def = find_class_def cls_name in
        let rec find_familly c = 
          try 
            List.assoc field_name c.attributes
          with Not_found ->
            match c.parent with
            | Some parentname -> 
              let parentclsdef = find_class_def parentname in
              find_familly parentclsdef
            | None -> error ("Field " ^ field_name ^ " not declared for class " ^ cls_name)
        in
        find_familly class_def
        )
        
  
  and check_instr i ret tenv : instr=
    match i with
    | Print e ->
        let typed_e = check_expr e tenv in 
        typed_e.annot |> check_eq_type TInt; Print(typed_e) 
    | If (cond, ifseq, elseseq) -> (
        let typed_cond = check_expr cond tenv in
        typed_cond.annot |> check_eq_type TBool;
        If(typed_cond, check_seq ifseq TVoid tenv, check_seq elseseq TVoid tenv))
    | While (cond, iseq) ->
        let typed_cond = check_expr cond tenv in
        typed_cond.annot |> check_eq_type TBool;
        While(typed_cond , check_seq iseq TVoid tenv )
    | Set (m, e) -> let typed_e = check_expr e tenv in
      typed_e.annot |> check_subtype (type_mem_access m tenv) ;  Set(m,typed_e)
    | Return e ->
        let typed_e = check_expr e tenv in
        check_eq_type typed_e.annot ret;
        Return typed_e
    | Expr e -> let typed_e = check_expr e tenv in
                  check_eq_type typed_e.annot TVoid;
                  Expr e
  and check_seq s ret tenv : seq = List.map (fun i -> check_instr i ret tenv) s in
  let typed_seq = check_seq p.main TVoid tenv in 

  let typed_classes = 
    List.map (
    fun c -> 
    let tenv = Env.empty in
    let tenv = add_env c.attributes tenv in
    let tenv = add_env [("this", TClass c.class_name)] tenv in
    {
      class_name = c.class_name;
      attributes = c.attributes;
      methods = 
    (List.map (fun m ->
      let tenv = add_env m.locals tenv in
      let tenv = add_env m.params tenv in
      {method_name = m.method_name ; code = check_seq (m.code) m.return tenv; params = m.params ; locals = m.locals ; return = m.return}) 
      c.methods);

      parent = c.parent
    }) 
    p.classes in

  {classes = typed_classes ; globals = p.globals ; main = typed_seq}

  