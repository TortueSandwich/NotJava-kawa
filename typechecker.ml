open Kawa
open Tools
exception TypeError of string

let error s = raise (TypeError s)

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


let check_subtype objective curr (find_class_def: string -> class_def)=
  let rec aux objective curr (find_class_def : string -> class_def) =
  if objective = curr then ()
  else match curr with
  | TClass name -> 
      let classdef = find_class_def name in
      begin match classdef.parent with
      | Some parentname -> 
          let parentclsdef = find_class_def parentname in
          aux objective (TClass parentclsdef.class_name) find_class_def
      | None -> error ("No parent class found for " ^ name)
      end
  | _ -> error ("Cannot check subtype for primitive type: " ^ typ_to_string curr)
  in
  try (aux objective curr find_class_def)
  with TypeError s -> error (typ_to_string(curr) ^" is not a subtype of "^typ_to_string(objective) ^ ", " ^ s) 

let find_class_def class_name classes =
  try List.find (fun cls -> cls.class_name = class_name) classes
  with Not_found -> let classes_names = List.map (fun cls -> cls.class_name) classes in
     error ("Class not found: " ^ class_name ^ (match closest_string class_name classes_names with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> ""))

let find_method_def meth_name methods =
  match List.find_opt (fun m -> m.method_name = meth_name) methods with
  | Some m -> m
  | None -> error ("Method not found: " ^ meth_name)

let objname_of_typ = function TClass clsname -> clsname | _ -> assert false

let typecheck_prog (p : program) f: program =
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
            {annot = TInt; expr = Unop(u, typed_e); loc = e.loc}
        | Not ->
            check_eq_type TBool typed_e.annot;
            {annot = TBool; expr = Unop(u,typed_e) ; loc = e.loc}
        | TypeCast (newType) -> 
            (try 
              check_subtype newType typed_e.annot ; {annot = newType ; expr = typed_e.expr}
            with
            | _ ->  
              (try check_subtype typed_e.annot newType ;{annot = newType ; expr = typed_e.expr}
              with TypeError s -> error ("Impossible to typecast, "^s)
              )
            )
        | InstanceOf (_) -> {annot = TBool ; expr = Unop(u, typed_e)}
    )
    | Binop (u, e1, e2) -> (
        let typed_e1 = check_expr e1 env_stack in
        let typed_e2 = check_expr e2 env_stack in
        match u with
        | Eq ->
            check_eq_type typed_e1.annot typed_e2.annot;
            { annot = TBool; expr = Binop (u, typed_e1, typed_e2) ; loc = e.loc}
        | Lt | Le | Gt | Ge | Neq ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            { annot = TBool; expr = Binop (u, typed_e1, typed_e2) ; loc = e.loc}
        | Add | Sub | Mul | Div | Rem ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            { annot = TInt; expr = Binop (u, typed_e1, typed_e2) ; loc = e.loc}
        | And | Or ->
            check_eq_type TBool typed_e1.annot;
            check_eq_type TBool typed_e2.annot;
            {annot = TBool ; expr =  Binop(u , typed_e1, typed_e2); loc = e.loc})
    | Get m -> {annot =  type_mem_access m tenv ; expr = e.expr}
    | This -> begin 
        try
          let this = Env.find env_stack "this" in
        (* TClass (c.class_name) *)
        {annot = c ; expr = e.expr ; loc = e.loc}
        with Not_found -> error ("Class not found: " ^ "this")
    end

    | New class_name -> 
      ignore(find_class_def class_name);     (*checks existance*)
      {annot = TClass class_name ; expr = e.expr; loc = e.loc}
    | NewCstr (class_name, args) ->
        let defclass = find_class_def class_name in
        let constructor = find_method_def "constructor" defclass.methods in
        let param_types = List.map snd constructor.params in
        let arg_types =
          List.map (fun arg -> (check_expr arg env_stack).annot) args
        in
        List.iter2 check_subtype param_types arg_types;
        { annot = TClass class_name; expr = e.expr; loc = e.loc }
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
          expr = MethCall (typed_obj, meth_name, typed_args); loc = e.loc;
        }
  and type_mem_access m stack_env : typ =
    match m with
    | Var name -> begin
      try Env.find stack_env name with
        | _ -> let closest = closest_string name (keys_of_map tenv) in error ("Undeclared variable: " ^ name ^ (if closest <> "" then
          ", did you mean " ^ (Option.get closest) ^ " ?\n"
        )
        end
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
    try
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
        {instr = Return typed_e; loc = i.loc}
    | Expr e ->
        let typed_e = check_expr e stack_env in
        check_eq_type typed_e.annot TVoid;
        {instr = Expr e; loc = i.loc}
    | Scope instrs ->
        let stack_env = Env.new_env stack_env in
        let typed_instrs = check_seq instrs TVoid stack_env in
        let _ = Env.pop_local_env stack_env in
        {instr = Scope typed_instrs; loc=i.loc}
    | Declare (varnames, t, None) ->
        let f x =
          if Env.is_declared_locally stack_env x then ()
          else Env.define_locally stack_env x t
        in
        List.iter f varnames;
        {instr = Declare (varnames, t, None); loc=i.loc}
    | Declare (varnames, t, Some v) ->
        let f x =
          if Env.is_declared_locally stack_env x then
            (* exec (Set (Var x, v)) stack_env *) ()
          else Env.define_locally stack_env x t
        in
        List.iter f varnames;
        let typedval = check_expr v stack_env in
        check_subtype t typedval.annot;
        {instr = Declare (varnames, t, Some typedval); loc=i.loc}

      with TypeError s -> let line  = (fst(i.loc)).pos_lnum in 
      error (s ^ "\nAt line " ^ string_of_int line ^"::   " ^ (get_string_from_file f i.loc))

      
  and check_seq s ret tenv : seq =
    List.map (fun i -> check_instr i ret tenv) s
  in

  let typed_seq = check_seq p.main TVoid (Env.new_env_stack ()) in

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

    List.map typed_one_class p.classes
  in

  { classes = typed_classes; globals = p.globals; main = typed_seq }
