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
  | AccessArray _ -> TInt  

let type_of_binop = function
  | Add | Sub | Mul | Div | Rem -> TInt
  | Lt | Le | Gt | Ge | Eq | Neq | And | Or | StructEq | NegStructEq-> TBool

(** ctx, expected, actual *)
let check_eq_type ?(context = "") expected actual =
  if expected <> actual then type_error ~context actual expected

let get_array_elem_type = function
  | TArray t -> t
  | _ -> error ("Not an array")

let rec get_array_core_type = function
  | TArray t -> get_array_core_type t
  | t -> t

let rec reduce_dim t = function
  | [] -> t
  | _ :: tl ->match t with TArray t -> reduce_dim t tl | _ -> error "Dimension mismatch"

let check_subtype objective curr (find_class_def: string -> class_def)=
  let rec aux objective curr (find_class_def : string -> class_def) =
  if objective = curr then ()
  else match curr with
  | TClass (name, _) -> 
      let classdef = find_class_def name in
      begin match classdef.parent with
      | Some parentname -> 
          let parentclsdef = find_class_def parentname in
          aux objective (TClass (parentclsdef.class_name, [])) find_class_def
      | None -> error ("No parent class found for " ^ name)
      end
  | _ -> error ("Cannot check subtype for primitive type: " ^ (Kawa.string_of_typ curr))
  in
  try (aux objective curr find_class_def)
  with TypeError s -> error (string_of_typ(curr) ^" is not a subtype of "^string_of_typ(objective) ^ ", " ^ s) 

let find_class_def class_name classes =
  try List.find (fun cls -> cls.class_name = class_name) classes
  with Not_found -> let classes_names = List.map (fun cls -> cls.class_name) classes in
     error ("Class not found: " ^ class_name ^ (match closest_string class_name classes_names with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> ""))

let find_interface_def interface_name interfaces =
  try List.find (fun i -> i.interface_name = interface_name) interfaces
  with Not_found -> error ("Interface not found: " ^ interface_name ^ (match closest_string interface_name (List.map (fun i -> i.interface_name) interfaces) with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> ""))

let find_method_def meth_name methods =
  match List.find_opt (fun m -> m.method_name = meth_name) methods with
  | Some m -> m
  | None -> error ("Method not found: " ^ meth_name ^ (match closest_string meth_name (List.map (fun m -> m.method_name) methods) with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> ""))

let objname_of_typ = function TClass (clsname, gener) -> clsname | _ -> assert false

let rec realtypeofgeneric clssdef genericstypes vartype = 
  let create_hashtbl keys values =
    let table = Hashtbl.create (List.length keys) in
    (try
    List.iter2 (fun key value -> Hashtbl.add table key value) keys values;
    with _ -> print_endline "err"
      );
    table
  in 
  let table = create_hashtbl clssdef.generics genericstypes in
  let res = match vartype with
    | TClass(pramnametype, _) -> 
       (Hashtbl.find_opt table pramnametype |> Option.value ~default:vartype) 
    | _ -> vartype
  in 
  let res = match res with
  | TClass(n, g) -> 
    TClass(n, 
    List.map (fun x ->  match x with
      | TClass(pramnametype, _) -> realtypeofgeneric clssdef genericstypes x
      | _ -> x
      ) 
      g)
  | _ -> res
  in 
    res

let keys_of_map map : string list =
  Hashtbl.fold (fun key _ acc -> key :: acc) map []

let typecheck_prog (p : program) : program =
  List.iter (fun (x, t) -> Env.define_globally x t) p.globals;

  let find_class_def class_name = find_class_def class_name p.classes in
  let find_interface_def interface_name = find_interface_def interface_name p.interfaces in
  let get_interfaces_from_class class_name = let c = find_class_def class_name in List.fold_left (fun acc name -> (find_interface_def name)::acc) [] c.implemented_interfaces in
  let check_subtype objective curr =
    check_subtype objective curr find_class_def
  in

  let rec check_expr (e : expr) env_stack : expr =
    (* print_endline ("typechheckexpr : " ^ string_of_expr e); *)
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
              check_subtype newType typed_e.annot ; {annot = newType ; expr = typed_e.expr; loc = e.loc}
            with
            | _ ->  (try check_subtype typed_e.annot newType ;{annot = newType ; expr = Unop(u, typed_e); loc = e.loc} (*typecast vers le bas, à vérifier à l'exec*)
                    with TypeError s -> error ("Impossible to typecast, "^s)
                      )
            )
        | InstanceOf (t) -> if t = typed_e.annot then {annot = TBool ; expr = Bool(true); loc = e.loc}
                            else {annot = TBool ; expr = Unop(u, typed_e); loc = e.loc}
        | AccessArray (index) -> let typed_index = check_expr index env_stack in 
                                 check_eq_type TInt typed_index.annot;
                                {annot = get_array_elem_type typed_e.annot; expr = Unop(u, typed_e); loc = e.loc} 
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
            {annot = TBool ; expr =  Binop(u , typed_e1, typed_e2); loc = e.loc}
      | StructEq | NegStructEq -> try check_eq_type typed_e1.annot typed_e2.annot;  { annot = TBool; expr = Binop (u, typed_e1, typed_e2) ; loc = e.loc}
                          with TypeError s ->  error ("Impossible to compare the two objects.\n" ^ s ^ report_bug e.loc (fst(e.loc)).pos_fname))
    | Get m -> {annot = type_mem_access m env_stack ; expr = e.expr; loc = e.loc} 
    | This -> begin 
        try
          let c = Env.find env_stack "this" in
          {annot = c ; expr = e.expr ; loc = e.loc}
        with Not_found -> error ("Class not found: " ^ "this")
    end

    | New (class_name, generics) -> 
      ignore(find_class_def class_name);     (*checks existance*)
      {annot = TClass (class_name, generics) ; expr = e.expr; loc = e.loc}
    | NewCstr (class_name, genrics ,args) ->
        let defclass = find_class_def class_name in
        let constructor = find_method_def "constructor" defclass.methods in
        let param_types = List.map snd constructor.params in
        let arg_types =
          List.map (fun arg -> (check_expr arg env_stack).annot) args
        in
        let check_subtype_spe paramt argt =
          let real = realtypeofgeneric defclass genrics paramt in
          check_subtype real argt
        in
        List.iter2 check_subtype_spe param_types arg_types;
        { annot = TClass (class_name, genrics); expr = e.expr; loc = e.loc }
    | MethCall (obj, meth_name, args) ->
        let typed_obj = check_expr obj env_stack in
        let typcls = objname_of_typ typed_obj.annot in
        let defclass = find_class_def typcls in
        let definterfaces = get_interfaces_from_class typcls in 
        let methodeu = find_method_def meth_name (defclass.methods@(List.filter (fun x -> x.default = true) (List.flatten (List.map (fun inter-> inter.methods) definterfaces)))) in
        let param_types = List.map snd methodeu.params in
        let typed_args = List.map (fun arg -> check_expr arg env_stack) args in
        
        
        let genapplication = match typed_obj.annot with 
          | TClass (_, g) -> g
          | _ -> assert false
        in
        
        let check_subtype_spe paramt argt =
          let real = realtypeofgeneric defclass genapplication paramt in
          check_subtype real argt
        in
        List.iter2 check_subtype_spe param_types
          (List.map (fun arg -> arg.annot) typed_args);
        let forreal = realtypeofgeneric defclass genapplication methodeu.return in
        {
          annot = forreal;
          expr = MethCall (typed_obj, meth_name, typed_args); loc = e.loc;
        }
      
      | SuperCall(meth_name, args) -> (
        try
        let c = Env.find env_stack "this" in
        let defclass = find_class_def (objname_of_typ c) in
        let parentdef = find_class_def (match defclass.parent with
        | Some parentname -> parentname
        | None -> error ("No parent class found for " ^ defclass.class_name ^ " cannot call super"))
        in
        let method_def = find_method_def meth_name parentdef.methods in
        let param_types = List.map snd method_def.params in
        let typed_args = List.map (fun arg -> check_expr arg env_stack) args in
        List.iter2 check_subtype param_types
          (List.map (fun arg -> arg.annot) typed_args);
        {
          annot = method_def.return;
          expr = SuperCall(meth_name, typed_args); loc = e.loc;
        }
 
        with Not_found -> error ("Super cannot be called in main")
        |     TypeError s -> error ("Super cannot be called, " ^ s )
      )
      | NewArray (t, n) ->
          let typed_n = List.map (fun x -> check_expr x env_stack) n in
          List.iter (fun x -> check_eq_type TInt x.annot) typed_n;
          {annot = e.annot; expr = NewArray (t, typed_n); loc = e.loc}
  and type_mem_access m stack_env : typ =
    match m with
    | Var name -> begin
      try 
        Env.find stack_env name 
      with
        | _ -> (
          let closest = closest_string name (Env.get_all_names stack_env) in 
          error ("Undeclared variable: " ^ name ^ (if closest <> (Some("")) then
            ", did you mean " ^ (Option.get closest) ^ " ?\n" else ""
            )
        ))
        end
    | Field (obj, field_name) ->
        let objtpye = (check_expr obj stack_env).annot in
        let cls_name = objname_of_typ objtpye in
        let class_def = find_class_def cls_name in
        let genericsapplication = match objtpye with 
        | TClass(_, g) -> g
        | _ -> assert false
        in
        let rec find_familly c =
          try 
            let (_,res) = List.find (fun (k, _) -> k = field_name) c.attributes in
            let res = realtypeofgeneric class_def genericsapplication res in (
              res
            )
            (* List.assoc field_name c.attributes *)
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
    | Array_var (name, l) -> try 
        let t = Env.find stack_env name in 
        reduce_dim t l
    with
    | _ -> (
      let closest = closest_string name (Env.get_all_names stack_env) in 
      error ("Undeclared variable: " ^ name ^ (if closest <> (Some("")) then
        ", did you mean " ^ (Option.get closest) ^ " ?\n" else ""
        )
    ))

  and check_instr i ret stack_env : instr =
    try
      (* print_endline ("typechheck : " ^ string_of_instr i.instr); *)
    match i.instr with
    | Print e ->
        let typed_e = check_expr e stack_env in
        check_eq_type TInt typed_e.annot;
        {instr=Print typed_e; loc= i.loc}
    | If (cond, ifseq, elseseq) ->
        let typed_cond = check_expr cond stack_env in
        check_eq_type TBool typed_cond.annot;
        {instr=If( typed_cond,
            check_seq ifseq TVoid stack_env,
            check_seq elseseq TVoid stack_env ); loc=i.loc}
    | While (cond, iseq) ->
        let typed_cond = check_expr cond stack_env in
        check_eq_type TBool typed_cond.annot;
        {instr=While (typed_cond, check_seq iseq TVoid stack_env);loc=i.loc}
    | Set (m, e) ->
        let typed_e = check_expr e stack_env in
        check_subtype (type_mem_access m stack_env) typed_e.annot ;
        {instr=Set (m, typed_e); loc= i.loc}
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
          if Env.is_declared_locally stack_env x then ()
          else Env.define_locally stack_env x t
        in
        List.iter f varnames;
        let typedval = check_expr v stack_env in
        check_subtype t typedval.annot;
        {instr = Declare (varnames, t, Some typedval); loc=i.loc}

      with TypeError s -> error (s)

      
  and check_seq s ret tenv : seq =
    List.map (fun i -> check_instr i ret tenv) s
  in

  let typed_classes =
    let class_match_interface (class_def:class_def) (interface_def:interface_def) : unit =
      let check_eq_signatures method1 method2 =
        if method1.params <> method2.params || method1.method_name <> method2.method_name || method1.return <> method2.return  then
          error
            (Printf.sprintf
               "Method %s has different signature than method %s"
               method1.method_name  method2.method_name) 
      in
      List.iter (fun meth -> try  check_eq_signatures (List.find (fun x -> meth.method_name = x.method_name) class_def.methods)  meth with Not_found  -> error (Printf.sprintf "Method %s must be implemented in class %s" meth.method_name class_def.class_name))
                 (List.filter (fun met -> met.default = false) interface_def.methods) (*checks implementation for methods without default*)
    in

    let typed_one_class c =
      let global_env = Env.new_env_stack () in
      let class_stack_env = Env.new_env global_env  in
      List.iter
        (fun (x, t) -> Env.define_locally class_stack_env x t)
        c.attributes;
      Env.define_locally class_stack_env "this" (TClass (c.class_name, List.map (fun x -> TClass(x, [])) c.generics));
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
          default = m.default;
        } 
      in
      
      List.iter (class_match_interface c) (List.map find_interface_def c.implemented_interfaces) ;
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

  let typed_seq = check_seq p.main TVoid (Env.new_env_stack ()) in

  { classes = typed_classes;interfaces = p.interfaces ;globals = p.globals; main = typed_seq }
