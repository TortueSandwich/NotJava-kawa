open Kawa
open Tools
open Find

type error =
  | DimensionMismatch
  | VariableNotFound of string * string list
  | NoParent of string (* 🙏 *)
  | SuperMain
  | UnAutorizeAccess of string * visibility
  | NotImplemented of class_def * method_def
  | NotIndexable of typ
  | UnexpectedType of typ (* expecteted*) * typ (*found *)
  | SubTypeError of typ * typ
  | PrimitiveTypeCast of typ
  | DifferentSignature of method_def * method_def
  | ClassNotFound of string * string list
  | InterfaceNotFound of string * string list
  | MethodNotFound of string * string list
  | AttributNotFoud of string * string list
(* | AlreadyDeclared of string *)

exception TpError of error * loc option

let localized_tpraise e loc = raise (TpError (e, Some loc))
let tpraise e = raise (TpError (e, None))

module ValueType = struct
  type t = typ

  let string_of_value = Kawa.string_of_typ
end

module Env = Stack_env.MakeEnv (ValueType)

(** RAISE NotIndexable si type nonindexable*)
let array_of_value t =
  match t with TArray a -> a | _ -> NotIndexable t |> tpraise

let rec elem_type = function TArray t -> elem_type t | t -> t

(*
  List.find (fun i -> i.interface_name = interface_name) interfaces
  with Not_found -> raise (NotFound (
  "Interface not found: " ^ interface_name ^ (
  match closest_string interface_name (List.map (fun i -> i.interface_name) interfaces) with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> ""))) *)

let objname_of_typ = function
  | TClass (clsname, gener) -> clsname
  | _ -> assert false

(** main attraction *)
let typecheck_prog (p : program) : program =
  let find_class_def loc =
    try find_class_def p
    with Find.FError (ClassNotFound (n, other)) ->
      localized_tpraise (ClassNotFound (n, other)) loc 
  in
  let find_parent_class_def loc =
    try find_parent_class_def p
    with Find.FError (ClassNotFound (n, other)) ->
      localized_tpraise (ClassNotFound (n, other)) loc
  in
  let find_method_locally_def loc =
    try find_method_locally_def p
    with Find.FError (MethodNotFound (n, other)) ->
      raise (TpError ((MethodNotFound (n, other)), loc))
  in
  (* Errors conversion *)
  let env_get loc env name =
    try Env.find env name
    with Stack_env.EnvError (UndefinedVariable s) ->
      localized_tpraise (VariableNotFound(name, Env.get_all_names env)) loc
  in

  let declare_locally loc env x t =
    if Env.is_declared_locally env x then ()
      (* localized_tpraise (AlreadyDeclared x) loc *)
    else Env.define_locally env x t
  in

  let ( <: ) a b = Find.check_subtype p b a in

  let rec check_expr (e : expr) env_stack : expr =
    let derive_expr annot expr = { annot; expr; loc = e.loc } in
    let tpraise err = localized_tpraise err e.loc in
    let ( <:? ) a b = if not (a <: b) then UnexpectedType (a, b) |> tpraise in
    let env_get = env_get e.loc in
    let find_class_def = find_class_def e.loc in
    let find_parent_class_def = find_parent_class_def e.loc in
    let find_method_locally_def = find_method_locally_def (Some e.loc) in

    let check_method_args defclass genrics param_types args =
      let arg_types =
        List.map (fun arg -> (check_expr arg env_stack).annot) args
      in
      let check_subtype_spe paramt argt =
        let real = realtypeofgeneric defclass genrics paramt in
        real <:? argt
      in
      List.iter2 check_subtype_spe param_types arg_types
    in

    (* print_endline ("typechheckexpr : " ^ string_of_expr e); *)
    match e.expr with
    | Int _ -> e
    | Bool _ -> e
    | Unop (u, e) -> (
        let typed_e = check_expr e env_stack in
        let resexpr = Unop (u, typed_e) in
        match u with
        | Opp ->
            typed_e.annot <:? TInt;
            derive_expr TInt resexpr
        | Not ->
            typed_e.annot <:? TBool;
            derive_expr TBool resexpr
        | TypeCast newType -> (
            if is_primitive newType then PrimitiveTypeCast newType |> tpraise;
            try
              typed_e.annot <:? newType;
              derive_expr newType typed_e.expr
            with _ ->
              (*typecast vers le bas, à vérifier à l'exec *)
              newType <:? typed_e.annot;
              derive_expr newType resexpr)
        | InstanceOf t ->
            if t = typed_e.annot then derive_expr TBool (Bool true)
            else derive_expr TBool resexpr)
    | Binop (u, e1, e2) -> (
        let typed_e1 = check_expr e1 env_stack in
        let typed_e2 = check_expr e2 env_stack in
        let resexpr = Binop (u, typed_e1, typed_e2) in
        match u with
        | Eq ->
            typed_e2.annot <:? typed_e1.annot;
            derive_expr TBool resexpr
        | Lt | Le | Gt | Ge | Neq ->
            typed_e1.annot <:? TInt;
            typed_e2.annot <:? TInt;
            derive_expr TBool resexpr
        | Add | Sub | Mul | Div | Rem ->
            typed_e1.annot <:? TInt;
            typed_e2.annot <:? TInt;
            derive_expr TInt resexpr
        | And | Or ->
            typed_e1.annot <:? TBool;
            typed_e2.annot <:? TBool;
            derive_expr TBool resexpr
        | StructEq | NegStructEq ->
            typed_e2.annot <:? typed_e1.annot;
            derive_expr TBool resexpr)
    | Get m -> derive_expr (type_mem_access m env_stack e.loc) e.expr
    | This ->
        let c = env_get env_stack "this" in
        derive_expr c e.expr
    | New (class_name, generics) ->
        (* existance *)
        ignore (find_class_def class_name);
        derive_expr (TClass (class_name, generics)) e.expr
    | NewCstr (class_name, genrics, args) ->
        let defclass = find_class_def class_name in
        let constructor = find_method_locally_def defclass "constructor" in
        let param_types = List.map snd constructor.params in
        check_method_args defclass genrics param_types args;
        derive_expr (TClass (class_name, genrics)) e.expr
    | MethCall (obj, meth_name, args) ->
        let typed_obj = check_expr obj env_stack in
        let defclass = find_class_def (objname_of_typ typed_obj.annot) in
        let methodeu = find_method_locally_def defclass meth_name in
        let param_types = List.map snd methodeu.params in
        let typed_args = List.map (fun arg -> check_expr arg env_stack) args in
        let genapplication =
          match typed_obj.annot with TClass (_, g) -> g | _ -> assert false
        in
        check_method_args defclass genapplication param_types typed_args;
        let forreal =
          realtypeofgeneric defclass genapplication methodeu.return
        in
        {
          annot = forreal;
          expr = MethCall (typed_obj, meth_name, typed_args);
          loc = e.loc;
        }
    | SuperCall (meth_name, args) -> (
        let c = env_get env_stack "this" in
        let defclass = find_class_def (objname_of_typ c) in
        let parentdef = find_parent_class_def defclass in
        match parentdef with
        | None -> NoParent defclass.class_name |> tpraise
        | Some parentdef ->
            let method_def = find_method_locally_def parentdef meth_name in
            let param_types = List.map snd method_def.params in
            let typed_args =
              List.map (fun arg -> check_expr arg env_stack) args
            in
            List.iter2 ( <:? ) param_types
              (List.map (fun arg -> arg.annot) typed_args);
            derive_expr method_def.return (SuperCall (meth_name, typed_args)))
    | NewArray (t, n) ->
        let typed_n = List.map (fun x -> check_expr x env_stack) n in
        List.iter (fun x -> x.annot <:? TInt) typed_n;
        derive_expr e.annot (NewArray (t, typed_n))
  and type_mem_access m stack_env loc : typ =
    let tpraise err = localized_tpraise err loc in
    let ( <:? ) a b = if not (a <: b) then UnexpectedType (a, b) |> tpraise in
    let env_get = env_get loc in

    let rec reduce_dim t = function
      | [] -> t
      | hd :: tl -> (
          (check_expr hd stack_env).annot <:? TInt;
          match t with
          | TArray t -> reduce_dim t tl
          | _ -> DimensionMismatch |> tpraise)
    in
    match m with
    | Var name ->
        env_get stack_env name
        (* let closest = closest_string name (Env.get_all_names stack_env) in
          let s = name ^ if closest <> Some "" then ", did you mean " ^ Option.get closest ^ " ?\n" else "" *)
    | Field (obj, field_name) ->
        let obj = check_expr obj stack_env in
        let { annot = objtpye; expr = eou } = obj in
        let cls_name = objname_of_typ objtpye in
        let class_def = find_class_def loc cls_name in
        let genericsapplication =
          match objtpye with TClass (_, g) -> g | _ -> assert false
        in
        let rec find_familly c courrant =
          try
            let _, res, visible = find_attribut_locally c field_name in
            (if eou <> This then
               match visible with
               | Public -> ()
               | Private -> UnAutorizeAccess (field_name, visible) |> tpraise
               | Protected ->
                   if courrant then ()
                   else UnAutorizeAccess (field_name, visible) |> tpraise);
            realtypeofgeneric class_def genericsapplication res
          with _ as e -> (
            let parentdef = find_parent_class_def loc c in
            match parentdef with
            | Some parentdef -> find_familly parentdef false
            | None -> raise e)
        in
        find_familly class_def true
    | Array_var (name, l) ->
        let t = type_mem_access name stack_env loc in
        reduce_dim t l
  (* let closest = closest_string name (Env.get_all_names stack_env) in  *)
  (* raise (NotFound("Undeclared variable: " ^ name ^ (if closest <> (Some("")) then *)
  (* ", did you mean " ^ (Option.get closest) ^ " ?\n" else ""))) *)
  (* ) *)
  and check_instr i ret stack_env : instr =
    let derive_instr newi = { instr = newi; loc = i.loc } in
    let tpraise err = localized_tpraise err i.loc in
    let ( <:? ) a b = if not (a <: b) then UnexpectedType (a, b) |> tpraise in

    match i.instr with
    | Print e ->
        let typed_e = check_expr e stack_env in
        typed_e.annot <:? TInt;
        derive_instr (Print typed_e)
    | If (cond, ifseq, elseseq) ->
        let typed_cond = check_expr cond stack_env in
        typed_cond.annot <:? TBool;
        let ifseq = check_seq ifseq TVoid stack_env in
        let elseseq = check_seq elseseq TVoid stack_env in
        let newinstr = If (typed_cond, ifseq, elseseq) in
        derive_instr newinstr
    | While (cond, iseq) ->
        let typed_cond = check_expr cond stack_env in
        typed_cond.annot <:? TBool;
        let whileseq = check_seq iseq TVoid stack_env in
        let new_instr = While (typed_cond, whileseq) in
        derive_instr new_instr
    | Set (m, e) ->
        let typed_e = check_expr e stack_env in
        let expect_type = type_mem_access m stack_env i.loc in
        typed_e.annot <:? expect_type;
        derive_instr (Set (m, typed_e))
    | Return e ->
        let typed_e = check_expr e stack_env in
        ret <:? typed_e.annot;
        derive_instr (Return typed_e)
    | Expr e ->
        let typed_e = check_expr e stack_env in
        TVoid <:? typed_e.annot;
        derive_instr (Expr e)
    | Scope instrs ->
        (* creer le scope dans check_seq*)
        let typed_instrs = check_seq instrs TVoid stack_env in
        derive_instr (Scope typed_instrs)
    | Declare (varnames, t, None) ->
        let f x = declare_locally i.loc stack_env x t in
        List.iter f varnames;
        derive_instr (Declare (varnames, t, None))
    | Declare (varnames, t, Some v) ->
        let f x = declare_locally i.loc stack_env x t in
        List.iter f varnames;
        let typedval = check_expr v stack_env in
        t <:? typedval.annot;
        derive_instr (Declare (varnames, t, Some typedval))
  and check_seq s ret stack_env : seq =
    let stack_env = Env.new_env stack_env in
    let res = List.map (fun i -> check_instr i ret stack_env) s in
    let _ = Env.pop_local_env stack_env in
    res
  in

  let check_classes =
    (* check si les interfaces sont bien implementé *)
    (* todo y'a plus simple *)
    let class_match_interface class_def interface_def =
      let check_eq_signatures method1 method2 =
        if
          method1.params <> method2.params
          || method1.method_name <> method2.method_name
          || method1.return <> method2.return
        then DifferentSignature (method1, method2) |> tpraise
      in
      let meth_with_body =
        List.filter (fun met -> met.default = false) interface_def.methods
      in
      let f meth =
        try
          let tmp = find_method_locally_def None class_def meth.method_name in
          check_eq_signatures tmp meth
        with Find.FError (MethodNotFound (s, n)) ->
          NotImplemented (class_def, meth) |> tpraise
      in
      List.iter f meth_with_body
    in

    let typed_one_class c =
      (* creer l'env pour l'analyse de la classe *)
      let global_env = Env.new_env_stack () in
      let class_stack_env = Env.new_env global_env in
      List.iter
        (fun (x, t, _) -> Env.define_locally class_stack_env x t)
        c.attributes;
      Env.define_locally class_stack_env "this"
        (TClass (c.class_name, List.map (fun x -> TClass (x, [])) c.generics));
      let typed_method m =
        (* creer l'env pour l'analyse de la methode *)
        let method_stack = Env.new_env class_stack_env in
        List.iter (fun (x, t) -> Env.define_locally method_stack x t) m.locals;
        List.iter (fun (x, t) -> Env.define_locally method_stack x t) m.params;
        {
          method_name = m.method_name;
          code = check_seq m.code m.return method_stack;
          params = m.params;
          locals = m.locals;
          return = m.return;
          default = m.default;
        }
      in

      List.iter (class_match_interface c)
        (List.map
           (fun inter -> find_interface_def p inter)
           c.implemented_interfaces);
      {
        class_name = c.class_name;
        generics = c.generics;
        attributes = c.attributes;
        methods = List.map typed_method c.methods;
        parent = c.parent;
        implemented_interfaces = c.implemented_interfaces;
      }
    in
    List.map typed_one_class p.classes
  in

  List.iter (fun (x, t) -> Env.define_globally x t) p.globals;
  let main_stack = Env.new_env_stack () in
  let typed_seq = check_seq p.main TVoid main_stack in
  {
    classes = check_classes;
    interfaces = p.interfaces;
    globals = p.globals;
    main = typed_seq;
  }
