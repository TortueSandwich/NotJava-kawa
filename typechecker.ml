open Kawa
open Tools

type error = 
| TypeError
| IndexOutOfBounds
| DimensionMismatch
| TypeCastError

| VariableNotFound of string
| ClassNotFound of string
| InterfaceNotFound of string
| MethodNotFound of string
| FieldNotFound of string * string
| CompileTimeError
| NoParent of string (* 🙏 *)
| SuperMain
| UnAutorizeAccess of string * visibility
| NotImplemented of class_def * method_def

| NotIndexable of typ
| UnexpectedType of typ (** expecteted *) * typ (* found *) 
| SubTypeError of typ * typ
| PrimitiveSubtype 
| DifferentSignature of method_def * method_def

(* exception TypeError of string  *)
(* exception IndexOutOfBounds  *)
(* exception DimensionMismatch *)
(* exception TypeCastError of string *)
(* exception NotFound of string (* Class / var / method / interface *) *)
(* exception CompileTimeError of string *)

(* exception TypeCheckerError of exn * loc  *)
exception TpError of error
let tpraise e = raise (TpError e)

(* let wrap_error exn loc = raise (TypeCheckerError (exn , loc)) *)
(* let error s = raise (TypeError s) *)

module ValueType = struct
  type t = typ

  let string_of_value = Kawa.string_of_typ
end

module Env = Stack_env.MakeEnv (ValueType)

let type_error ?(context = "") ty_actual ty_expected =
  (* let context_msg = if context = "" then "" else " in " ^ context in *)
  UnexpectedType (ty_expected, ty_actual) |> tpraise
  (* error *)
    (* (Printf.sprintf "Error%s: expected %s, got %s" context_msg *)
       (* (string_of_typ ty_expected) *)
       (* (string_of_typ ty_actual)) *)


(** ctx, expected, actual *)
let check_eq_type ?(context = "") expected actual =
  if expected <> actual then type_error ~context actual expected

let (=?=) expect got = (expect = got)
(* let get_array_elem_type = function
  | TArray t -> t
  | _ -> error ("Not an array") *)

(** RAISE NotIndexable si type nonindexable*)
let array_of_value t =
  match t with TArray a -> a | _ -> raise (TpError (NotIndexable t))

let rec elem_type = function
  | TArray t -> elem_type t
  | t -> t

(** prog objective  curr 
a <: b <=> a extends b
*)
let check_subtype p soustypede trucquiextends =
  match soustypede, trucquiextends with 
  | TClass(soustypede, _), TClass(trucquiextends, _) -> (
    let soustypede_def = find_class_def p soustypede in
    let trucquiextends_def = find_class_def p trucquiextends in
    match soustypede_def, trucquiextends_def with
    | Some soustypede, Some trucquiextends -> 
      let familly = Kawa.get_familly p trucquiextends in 
      let verite = List.find_opt (fun x -> x=soustypede) familly in
      Option.is_some verite
    | None,_ -> ClassNotFound soustypede |> tpraise
    | _, None -> ClassNotFound trucquiextends |> tpraise
  )
  | _ -> PrimitiveSubtype  |> tpraise
  (* let rec aux objective curr (find_class_def : string -> class_def) =
  if objective = curr then ()
  else match curr with
  | TClass (name, _) -> 
      let classdef = find_class_def name in
      begin match classdef.parent with
      | Some parentname -> 
          let parentclsdef = find_class_def parentname in
          aux objective (TClass (parentclsdef.class_name, [])) find_class_def
      | None -> NotFound name |> tpraise
      end
  | _ -> error ("Cannot check subtype for primitive type: " ^ (Kawa.string_of_typ curr))
  in
  try (aux objective curr find_class_def)
  with TypeError s -> error (string_of_typ(curr) ^" is not a subtype of "^string_of_typ(objective) ^ ", " ^ s)  *)


let find_class_def p class_name =
  try 
    List.find (fun cls -> cls.class_name = class_name) p.classes
  with Not_found -> ClassNotFound class_name |> tpraise
  
let find_parent_class_def p son_name = 
  match son_name.parent with 
  | Some parentname -> find_class_def p parentname
  | None -> NoParent son_name.class_name |> tpraise
  

let rec get_familly p classdef =
  match classdef.parent with
  | None -> []
  | Some pname -> (
      let pardef = find_class_def p pname in
      pardef  :: get_familly p pardef)

let find_interface_def p interface_name = 
  try
    List.find (fun i -> i.interface_name = interface_name) p.interfaces
  with Not_found -> InterfaceNotFound interface_name |> tpraise
  

(* let find_interface_def interface_name interfaces =
  try 
  List.find (fun i -> i.interface_name = interface_name) interfaces
  with Not_found -> raise (NotFound ("Interface not found: " ^ interface_name ^ (match closest_string interface_name (List.map (fun i -> i.interface_name) interfaces) with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> ""))) *)

let get_class_methods p class_def =
  let definterfaces = List.fold_left (fun acc name -> (find_interface_def  p name)::acc) [] class_def.implemented_interfaces in
  let machin = (List.flatten (List.map (fun inter-> inter.methods) definterfaces)) in
  let other = (List.filter (fun x -> x.default = true) machin) in
  let what = (class_def.methods@other) in
  what

let rec find_method_def p defclass meth_name =
  let allmeth = (get_class_methods p defclass) in
  match List.find_opt (fun m -> m.method_name = meth_name) allmeth with
  | Some m -> m
  | None ->     
    (match defclass.parent with
    | Some parent -> 
      let parentdefclass = List.find (fun cls -> cls.class_name = parent) p.classes in
      find_method_def p parentdefclass meth_name
    | None -> MethodNotFound meth_name |> tpraise
      (* error ("Method not found: " ^ meth_name ^ (match closest_string meth_name (List.map (fun m -> m.method_name) allmeth) with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> "")) *)
  )

let find_method_def_locally (classdef:class_def) meth_name  =
  match List.find_opt (fun m -> m.method_name = meth_name) classdef.methods with
  | Some m -> m
  | None -> MethodNotFound meth_name |> tpraise
    (* raise (NotFound ("Method not found: " ^ meth_name ^ (match closest_string meth_name (List.map (fun m -> m.method_name) methods) with
     | Some closest -> ", did you mean " ^ closest ^ " ?\n"
     | None -> ""))) *)

let objname_of_typ = function TClass (clsname, gener) -> clsname | _ -> assert false

let create_generic_table (generic_ident: string list) (generic_type: typ list) =
  let n = List.length generic_ident in
  assert (n = List.length generic_type);
  let res = Hashtbl.create n in
  List.fold_left2 (fun table key value -> Hashtbl.add table key value; table) res generic_ident generic_type


let rec realtypeofgeneric classdef genericstypes vartype = 
  let gen_table = create_generic_table classdef.generics genericstypes in
  let res = match vartype with
    | TClass(pramnametype, _) -> 
       (Hashtbl.find_opt gen_table pramnametype |> Option.value ~default:vartype) 
    | _ -> vartype (* primitive type*)
  in 
  let res = match res with
  | TClass(n, g) -> 
    TClass(n, 
    List.map (fun x ->  match x with
      | TClass(pramnametype, _) -> realtypeofgeneric classdef genericstypes x
      | _ -> x
      ) 
      g)
  | _ -> res
  in 
    res

let keys_of_map map : string list =
  Hashtbl.fold (fun key _ acc -> key :: acc) map []


(** main attraction *)
let typecheck_prog (p : program) : program =
  (* let find_class_def class_name = find_class_def class_name p.classes in
  let find_interface_def interface_name = find_interface_def interface_name p.interfaces in
  
  let check_subtype objective curr =
    check_subtype objective curr find_class_def
  in *)

  let (<:) a b = check_subtype p b a in
  let (<:?) a b = if not (a <: b) then SubTypeError(a, b) |> tpraise in

  let rec check_expr (e : expr) env_stack : expr =
    let derive_expr annot expr = 
      {annot=annot; expr=expr; loc=e.loc}
    in
    (* print_endline ("typechheckexpr : " ^ string_of_expr e); *)
    match e.expr with
    | Int _ -> e
    | Bool _ -> e
    | Unop (u, e) -> (
        let typed_e = check_expr e env_stack in
        let resexpr = Unop(u, typed_e) in
        match u with
        | Opp ->
            check_eq_type TInt typed_e.annot;
            derive_expr TInt resexpr
        | Not ->
            check_eq_type TBool typed_e.annot;
            derive_expr TBool resexpr
        | TypeCast (newType) -> (try 
            newType <:? typed_e.annot;
              (* check_subtype p newType typed_e.annot ;  *)
              derive_expr newType typed_e.expr
            with
            | _ -> failwith "wtf todo"
              (* {annot = newType ; expr = resexpr; loc = e.loc} *)
              (* (try check_subtype typed_e.annot newType ;{annot = newType ; expr = resexpr; loc = e.loc} typecast vers le bas, à vérifier à l'exec *)
                    (* with TypeError s -> wrap_error (TypeCastError ("Impossible to typecast, "^s)) e.loc *)
                      (* ) *)
            )
        | InstanceOf (t) -> if t = typed_e.annot then derive_expr TBool (Bool(true))
          else derive_expr TBool resexpr 
    )
    | Binop (u, e1, e2) -> (
        let typed_e1 = check_expr e1 env_stack in
        let typed_e2 = check_expr e2 env_stack in
        let resexpr = Binop(u, typed_e1, typed_e2) in
        match u with
        | Eq ->
            check_eq_type typed_e1.annot typed_e2.annot;
            derive_expr TBool resexpr
        | Lt | Le | Gt | Ge | Neq ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            derive_expr TBool resexpr
        | Add | Sub | Mul | Div | Rem ->
            check_eq_type TInt typed_e1.annot;
            check_eq_type TInt typed_e2.annot;
            derive_expr TInt resexpr
        | And | Or ->
            check_eq_type TBool typed_e1.annot;
            check_eq_type TBool typed_e2.annot;
            derive_expr TBool resexpr
      | StructEq | NegStructEq -> (*try*)
          check_eq_type typed_e1.annot typed_e2.annot; 
          derive_expr TBool resexpr)
          (* with TypeError s -> wrap_error (error ("Impossible to compare the two objects.\n" ^ s)) e.loc ) *)
    | Get m -> derive_expr (type_mem_access m env_stack) e.expr
    | This -> begin 
        try
          let c = Env.find env_stack "this" in
          derive_expr c e.expr
        with Not_found -> VariableNotFound "this" |> tpraise
    end

    | New (class_name, generics) -> 
      ignore(find_class_def p class_name);     (*checks existance*)
      derive_expr (TClass (class_name, generics)) e.expr
    | NewCstr (class_name, genrics ,args) ->
        let defclass = find_class_def p class_name in
        let constructor = find_method_def_locally defclass "constructor"  in
        let param_types = List.map snd constructor.params in
        let arg_types =
          List.map (fun arg -> (check_expr arg env_stack).annot) args
        in
        let check_subtype_spe paramt argt =
          let real = realtypeofgeneric defclass genrics paramt in
          real <:? argt
        in
        List.iter2 check_subtype_spe param_types arg_types;
        derive_expr (TClass (class_name, genrics)) e.expr
    | MethCall (obj, meth_name, args) ->
        let typed_obj = check_expr obj env_stack in
        let typcls = objname_of_typ typed_obj.annot in
        let defclass = find_class_def p typcls in

        (* FAIRE fonction getallmethod classdef -> method list *)
      
        (* let allclassmeths = get_all_method defclass p.interfaces in *)
        let methodeu = find_method_def p defclass meth_name in
        let param_types = List.map snd methodeu.params in
        let typed_args = List.map (fun arg -> check_expr arg env_stack) args in
        
        
        let genapplication = match typed_obj.annot with 
          | TClass (_, g) -> g
          | _ -> assert false
        in
        
        let check_subtype_spe paramt argt =
          let real = realtypeofgeneric defclass genapplication paramt in
          real <:? argt
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
          let defclass = find_class_def p (objname_of_typ c) in
          let parentdef = find_parent_class_def p defclass in
          let method_def = find_method_def p parentdef meth_name in
          let param_types = List.map snd method_def.params in
          let typed_args = List.map (fun arg -> check_expr arg env_stack) args in
          List.iter2 (<:?) param_types
            (List.map (fun arg -> arg.annot) typed_args);
          derive_expr method_def.return (SuperCall(meth_name, typed_args))
        with Stack_env.EnvError _ -> SuperMain |> tpraise
              (* wrap_error (CompileTimeError ("Super cannot be called in main")) e.loc *)
        (* |    err -> wrap_error err e.loc   *)
      )
      | NewArray (t, n) ->
          let typed_n = List.map (fun x -> check_expr x env_stack) n in
          List.iter (fun x -> check_eq_type TInt x.annot) typed_n;
          derive_expr e.annot (NewArray(t, typed_n))
  and type_mem_access m stack_env : typ =
    let rec reduce_dim t = function
    | [] -> t
    | hd :: tl ->
      check_eq_type TInt (check_expr hd stack_env).annot; 
      (match t with TArray t -> reduce_dim t tl | _ ->  DimensionMismatch |> tpraise )
  in
    match m with
    | Var name -> begin
      try 
        Env.find stack_env name 
      with | _ -> (
          VariableNotFound name |> tpraise
          (* let closest = closest_string name (Env.get_all_names stack_env) in  *)
          (* raise (NotFound ("Undeclared variable: " ^ name ^ (if closest <> (Some("")) then *)
            (* ", did you mean " ^ (Option.get closest) ^ " ?\n" else "" *)
            (* )) *)
        (* ) *)
        )
        end
    | Field (obj, field_name) ->
        let obj = check_expr obj stack_env in
        let {annot=objtpye; expr=eou} = obj in
        let cls_name = objname_of_typ objtpye in
        let class_def = find_class_def p cls_name in
        let genericsapplication = match objtpye with 
        | TClass(_, g) -> g
        | _ -> assert false
        in
        let rec find_familly c courrant =
          try 
            let (_,res, visible) = List.find (fun (k, _, _) -> k = field_name) c.attributes in
            if eou <> This then 
            (match visible with 
            | Public -> ()
            | Private -> UnAutorizeAccess(field_name, visible) |> tpraise;
            | Protected -> if courrant then () else UnAutorizeAccess(field_name, visible) |> tpraise;
            ); 
            realtypeofgeneric class_def genericsapplication res 
          with Not_found -> (
            match c.parent with
            | Some parentname ->
                let parentclsdef = find_class_def p parentname in
                find_familly parentclsdef false
            | None ->
              FieldNotFound (field_name, cls_name) |> tpraise
                (* raise(NotFound
                  ("Field " ^ field_name ^ " not declared for class " ^ cls_name))) *)
          )
        in
        find_familly class_def true
    | Array_var (name, l) -> (try 
      let t = type_mem_access name stack_env in 
      reduce_dim t l 
  with
  | Stack_env.EnvError _ -> (
    (* let closest = closest_string name (Env.get_all_names stack_env) in  *)
    (* raise (NotFound("Undeclared variable: " ^ name ^ (if closest <> (Some("")) then *)
      (* ", did you mean " ^ (Option.get closest) ^ " ?\n" else ""))) *)
    (* ) *)
    failwith "todo" (*huh il est ou le try*)
  )
  | err -> raise err

    )
  and check_instr i ret stack_env : instr =
    (* try *)
      (* print_endline ("typechheck : " ^ string_of_instr i.instr); *)
      let derive_instr newi = {instr=newi; loc= i.loc} in
    match i.instr with
    | Print e ->
        let typed_e = check_expr e stack_env in
        check_eq_type TInt typed_e.annot;
        derive_instr (Print typed_e)
    | If (cond, ifseq, elseseq) ->
        let typed_cond = check_expr cond stack_env in
        check_eq_type TBool typed_cond.annot;
        let newinstr = If(typed_cond,
            check_seq ifseq TVoid stack_env,
            check_seq elseseq TVoid stack_env )in
        derive_instr newinstr
    | While (cond, iseq) ->
        let typed_cond = check_expr cond stack_env in
        check_eq_type TBool typed_cond.annot;
        let new_instr=While (typed_cond, check_seq iseq TVoid stack_env)in
        derive_instr new_instr
    | Set (m, e) ->
        let typed_e = check_expr e stack_env in
        (type_mem_access m stack_env) <:? typed_e.annot ;
        derive_instr (Set (m, typed_e))
    | Return e ->
        let typed_e = check_expr e stack_env in
        check_eq_type typed_e.annot ret;
        derive_instr(Return typed_e)
    | Expr e ->
        let typed_e = check_expr e stack_env in
        check_eq_type typed_e.annot TVoid;
        derive_instr (Expr e)
    | Scope instrs ->
        let stack_env = Env.new_env stack_env in
        let typed_instrs = check_seq instrs TVoid stack_env in
        let _ = Env.pop_local_env stack_env in
        derive_instr (Scope typed_instrs)
    | Declare (varnames, t, None) ->
        let f x =
          if Env.is_declared_locally stack_env x then ()
          else Env.define_locally stack_env x t
        in
        List.iter f varnames;
        derive_instr (Declare (varnames, t, None))
    | Declare (varnames, t, Some v) ->
        let f x =
          if Env.is_declared_locally stack_env x then ()
          else Env.define_locally stack_env x t
        in
        List.iter f varnames;
        let typedval = check_expr v stack_env in
        t <:? typedval.annot;
        derive_instr (Declare (varnames, t, Some typedval))

      (* with  *)
      (* TypeCheckerError (s,loc) -> raise (TypeCheckerError(s,loc)) *)
      (* | err -> wrap_error err i.loc *)

      
  and check_seq s ret tenv : seq =
    List.map (fun i -> check_instr i ret tenv) s
  in

  let typed_classes =
    let class_match_interface (class_def:class_def) (interface_def:interface_def) : unit =
      let check_eq_signatures method1 method2 =
        if method1.params <> method2.params || method1.method_name <> method2.method_name || method1.return <> method2.return  then
          DifferentSignature(method1, method2)|> tpraise
          (* error *)
            (* (Printf.sprintf *)
               (* "Method %s has different signature than method %s" *)
               (* method1.method_name  method2.method_name)  *)
      in
      let meth_with_body = (List.filter (fun met -> met.default = false) interface_def.methods) in
      let f meth = try  
        let tmp = find_method_def p class_def (meth.method_name) in
          (* (List.find (fun x -> meth.method_name = x.method_name) class_def.methods) in *)
        check_eq_signatures tmp meth 
        with Not_found  -> NotImplemented(class_def, meth)|> tpraise
          (* raise (CompileTimeError (Printf.sprintf "Method %s must be implemented in class %s" meth.method_name class_def.class_name))  *)
      in
      List.iter f meth_with_body (*checks implementation for methods without default*)
    in

    let typed_one_class c =
      let global_env = Env.new_env_stack () in
      let class_stack_env = Env.new_env global_env  in
      List.iter
        (fun (x, t, _) -> Env.define_locally class_stack_env x t)
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
      
      List.iter (class_match_interface c) (List.map (fun inter -> find_interface_def p inter) c.implemented_interfaces) ;
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
  { classes = typed_classes; interfaces = p.interfaces; globals = p.globals; main = typed_seq }
