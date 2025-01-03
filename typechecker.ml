open Kawa

exception Error of string

let error s = raise (Error s)

let type_error ?(context = "") ty_actual ty_expected =
  let context_msg = if context = "" then "" else " in " ^ context in
  error
    (Printf.sprintf "Error%s: expected %s, got %s" context_msg
       (string_of_typ ty_expected)
       (string_of_typ ty_actual))

module ValueType = struct
  type t = typ

  let string_of_value = Kawa.string_of_typ
end

module Env = Stack_env.MakeEnv (ValueType)

(* module Env = Map.Make (String) *)

(* type tenv = typ Env.t *)

let type_of_unop = function
  | Opp -> TInt
  | Not -> TBool
  | TypeCast newType -> newType
  | InstanceOf _ -> TBool

let type_of_binop = function
  | Add | Sub | Mul | Div | Rem -> TInt
  | Lt | Le | Gt | Ge | Eq | Neq | And | Or -> TBool

(** ctx, expected, actual *)
let check_eq_type ?(context = "") expected actual =
  if expected <> actual then type_error ~context actual expected

let rec check_subtype objective curr (find_class_def : string -> class_def) =
  if objective = curr then ()
  else
    match curr with
    | TClass name -> (
        let classdef = find_class_def name in
        match classdef.parent with
        | Some parentname ->
            let parentclsdef = find_class_def parentname in
            check_subtype objective (TClass parentclsdef.class_name)
              find_class_def
        | None -> error ("No parent class found for " ^ name))
    | _ ->
        error ("Cannot check subtype for primitive type: " ^ string_of_typ curr)

let find_class_def class_name classes =
  try List.find (fun cls -> cls.class_name = class_name) classes
  with Not_found -> error ("Class not found: " ^ class_name)

let find_method_def meth_name methods =
  match List.find_opt (fun m -> m.method_name = meth_name) methods with
  | Some m -> m
  | None -> error ("Method not found: " ^ meth_name)

let objname_of_typ = function TClass clsname -> clsname | _ -> assert false

let typecheck_prog (p : program) : program =
  List.iter (fun (x, t) -> Env.define_globally x t) p.globals;

  let find_class_def class_name = find_class_def class_name p.classes in
  let check_subtype objective curr =
    check_subtype objective curr find_class_def
  in

  let rec check_expr (e : expr) env_stack : expr =
    match e.expr with
    | Int _ -> e
    | Bool _ -> e
    | Unop (u, e) -> (
        let typed_e = check_expr e env_stack in
        match u with
        | Opp ->
            check_eq_type TInt typed_e.annot;
            typed_e
        | Not ->
            check_eq_type TBool typed_e.annot;
            typed_e
        | TypeCast newType -> (
            try
              check_subtype newType typed_e.annot;
              { annot = newType; expr = typed_e.expr }
            with _ ->
              check_subtype typed_e.annot newType;
              { annot = newType; expr = typed_e.expr })
        | InstanceOf _ -> { annot = TBool; expr = Unop (u, typed_e) })
    | Binop (u, e1, e2) -> (
        let typed_e1 = check_expr e1 env_stack in
        let typed_e2 = check_expr e2 env_stack in
        match u with
        | Eq ->
            check_eq_type typed_e1.annot typed_e2.annot;
            { annot = TBool; expr = Binop (u, typed_e1, typed_e2) }
        | Lt | Le | Gt | Ge | Neq ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            { annot = TBool; expr = Binop (u, typed_e1, typed_e2) }
        | Add | Sub | Mul | Div | Rem ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            { annot = TInt; expr = Binop (u, typed_e1, typed_e2) }
        | And | Or ->
            check_eq_type TBool typed_e1.annot;
            check_eq_type TBool typed_e2.annot;
            { annot = TBool; expr = Binop (u, typed_e1, typed_e2) })
    | Get m -> { annot = type_mem_access m env_stack; expr = e.expr }
    | This -> (
        try
          let this = Env.find env_stack "this" in
          { annot = this; expr = e.expr }
        with Not_found -> error ("Class not found: " ^ "this"))
    | New class_name ->
        (* todo check si la classe existe *)
        { annot = TClass class_name; expr = e.expr }
    | NewCstr (class_name, args) ->
        let defclass = find_class_def class_name in
        let constructor = find_method_def "constructor" defclass.methods in
        let param_types = List.map snd constructor.params in
        let arg_types =
          List.map (fun arg -> (check_expr arg env_stack).annot) args
        in
        List.iter2 check_subtype param_types arg_types;
        { annot = TClass class_name; expr = e.expr }
    | MethCall (obj, meth_name, args) ->
        let typed_obj = check_expr obj env_stack in

        let typcls = objname_of_typ typed_obj.annot in
        let defclass = find_class_def typcls in
        let methodeu = find_method_def meth_name defclass.methods in
        let param_types = List.map snd methodeu.params in
        let typed_args = List.map (fun arg -> check_expr arg env_stack) args in
        List.iter2 check_subtype param_types
          (List.map (fun arg -> arg.annot) typed_args);
        {
          annot = methodeu.return;
          expr = MethCall (typed_obj, meth_name, typed_args);
        }
  and type_mem_access m stack_env : typ =
    match m with
    | Var name -> Env.find stack_env name
    | Field (obj, field_name) ->
        let cls_name = objname_of_typ (check_expr obj stack_env).annot in
        let class_def = find_class_def cls_name in
        let rec find_familly c =
          try List.assoc field_name c.attributes
          with Not_found -> (
            match c.parent with
            | Some parentname ->
                let parentclsdef = find_class_def parentname in
                find_familly parentclsdef
            | None ->
                error
                  ("Field " ^ field_name ^ " not declared for class " ^ cls_name))
        in
        find_familly class_def
  and check_instr i ret stack_env : instr =
    match i with
    | Print e ->
        let typed_e = check_expr e stack_env in
        check_eq_type TInt typed_e.annot;
        Print typed_e
    | If (cond, ifseq, elseseq) ->
        let typed_cond = check_expr cond stack_env in
        check_eq_type TBool typed_cond.annot;
        If
          ( typed_cond,
            check_seq ifseq TVoid stack_env,
            check_seq elseseq TVoid stack_env )
    | While (cond, iseq) ->
        let typed_cond = check_expr cond stack_env in
        check_eq_type TBool typed_cond.annot;
        While (typed_cond, check_seq iseq TVoid stack_env)
    | Set (m, e) ->
        let typed_e = check_expr e stack_env in
        check_subtype typed_e.annot (type_mem_access m stack_env);
        Set (m, typed_e)
    | Return e ->
        let typed_e = check_expr e stack_env in
        check_eq_type typed_e.annot ret;
        Return typed_e
    | Expr e ->
        let typed_e = check_expr e stack_env in
        check_eq_type typed_e.annot TVoid;
        Expr e
    | Scope instrs ->
        let stack_env = Env.new_env stack_env in
        let typed_instrs = check_seq instrs TVoid stack_env in
        let _ = Env.pop_local_env stack_env in
        Scope typed_instrs
    | Declare (varnames, t, None) ->
        let f x =
          if Env.is_declared_locally stack_env x then ()
          else Env.define_locally stack_env x t
        in
        List.iter f varnames;
        Declare (varnames, t, None)
    | Declare (varnames, t, Some v) ->
        let f x =
          if Env.is_declared_locally stack_env x then
            (* exec (Set (Var x, v)) stack_env *) ()
          else Env.define_locally stack_env x t
        in
        List.iter f varnames;
        let typedval = check_expr v stack_env in
        check_subtype t typedval.annot;
        Declare (varnames, t, Some typedval)
  and check_seq s ret tenv : seq =
    List.map (fun i -> check_instr i ret tenv) s
  in

  let typed_seq = check_seq p.main TVoid (Env.new_env_stack ()) in

  let typed_classes =
    let typed_one_class c =
      let global_env = Env.new_env_stack () in
      let class_stack_env = Env.new_env global_env  in
      List.iter
        (fun (x, t) -> Env.define_locally class_stack_env x t)
        c.attributes;
      Env.define_locally class_stack_env "this" (TClass c.class_name);
      let typed_method m =
        let method_stack = Env.new_env class_stack_env in
        List.iter (fun (x, t) -> Env.define_locally method_stack x t) m.locals;
        List.iter (fun (x, t) -> Env.define_locally method_stack x t) m.params;
        {
          method_name = m.method_name;
          code = check_seq m.code m.return method_stack;
          params = m.params;
          locals = m.locals;
          return = m.return;
        } 
      in

      {
        class_name = c.class_name;
        attributes = c.attributes;
        methods = List.map typed_method c.methods;
        parent = c.parent;
      }
    in

    List.map typed_one_class p.classes
  in

  { classes = typed_classes; globals = p.globals; main = typed_seq }
