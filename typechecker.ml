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

type tenv = (string, typ) Hashtbl.t

let add_env l tenv = List.iter (fun (x, t) -> Hashtbl.add tenv x t) l
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

let typecheck_prog p =
  let tenv = Hashtbl.create 16 in
  add_env p.globals tenv ;
  let find_class_def class_name = find_class_def class_name p.classes in
  let check_subtype objective curr = check_subtype objective curr find_class_def in

  let rec check_expr e tenv : typ =
    match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Unop (u, e) -> (
        let type_e = check_expr e tenv in
        match u with
        | Opp ->
            check_eq_type TInt type_e;
            TInt
        | Not ->
            check_eq_type TBool type_e;
            TBool
        | TypeCast (newType) -> check_subtype newType type_e;
            type_e)

    | Binop (u, e1, e2) -> (
        let type_e1 = check_expr e1 tenv in
        let type_e2 = check_expr e2 tenv in
        match u with
        | Eq -> check_eq_type type_e1 type_e2; TBool
        | Lt | Le | Gt | Ge | Neq ->
            check_eq_type TInt type_e1;
            check_eq_type TInt type_e2;
            TBool
        | Add | Sub | Mul | Div | Rem ->
            check_eq_type TInt type_e1;
            check_eq_type TInt type_e2;
            TInt
        | And | Or ->
            check_eq_type TBool type_e1;
            check_eq_type TBool type_e2;
            TBool)
    | Get m -> type_mem_access m tenv
    | This -> begin 
        try 
        (* let c = List.find (fun cls -> cls.class_name = "this") tenv *)
        let c = Hashtbl.find tenv "this" 
        in 
        (* TClass (c.class_name) *)
        c
        with Not_found -> error ("Class not found: " ^ "this")
    end

    | New class_name -> 
      (* todo check si la classe existe *)
      TClass class_name
    | NewCstr (class_name, args) ->
        let defclass = find_class_def class_name in
        let constructor = find_method_def "constructor" defclass.methods in
        let param_types = List.map snd constructor.params in
        let arg_types = List.map (fun arg -> check_expr arg tenv) args in
        List.iter2 check_subtype param_types arg_types;
        TClass class_name

    | MethCall (obj, meth_name, args) ->
        let typcls = objname_of_typ (check_expr obj tenv) in
        let defclass = find_class_def typcls in
        let methodeu = find_method_def meth_name defclass.methods in
        let param_types = List.map snd methodeu.params in
        let arg_types = List.map (fun arg -> check_expr arg tenv) args in
        List.iter2 check_subtype param_types arg_types;
        methodeu.return

  and type_mem_access m tenv =
    match m with
    | Var name -> (
        match Hashtbl.find_opt tenv name with
        | Some typ -> typ
        | None -> error ("Undeclared variable: " ^ name))
    | Field (obj, field_name) -> (
        let cls_name = objname_of_typ (check_expr obj tenv) in
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
        
  
  and check_instr i ret tenv =
    match i with
    | Print e ->
        check_expr e tenv |> check_eq_type TInt
    | If (cond, ifseq, elseseq) ->
        check_expr cond tenv |> check_eq_type TBool;
        check_seq ifseq TVoid tenv;
        check_seq elseseq TVoid tenv
    | While (cond, iseq) ->
        check_expr cond tenv |> check_eq_type TBool;
        check_seq iseq TVoid tenv
    | Set (m, e) ->
      let type_e = check_expr e tenv in 
      check_subtype (type_mem_access m tenv) type_e; 
      (match m with Var name -> (Hashtbl.add tenv name type_e ) | _-> ())
    | Return e ->
        let typ_e = check_expr e tenv in
        check_eq_type typ_e ret
    | Expr e -> ignore (check_expr e tenv)
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s in
  check_seq p.main TVoid tenv;

  List.iter (fun c -> 
    let tenv = Hashtbl.create 16 in
    add_env c.attributes tenv ;
    add_env [("this", TClass c.class_name)] tenv ;
    (List.iter (fun m ->
      add_env m.locals tenv ;
      add_env m.params tenv ;
      check_seq (m.code) m.return tenv) 
      c.methods)) 
    p.classes

  