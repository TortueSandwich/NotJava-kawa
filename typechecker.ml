open Kawa

exception Error of string

let error s = raise (Error s)

let type_error ty_actual ty_expected =
  error
    (Printf.sprintf "expected %s, got %s"
       (typ_to_string ty_expected)
       (typ_to_string ty_actual))

module Env = Map.Make (String)

type tenv = typ Env.t

let add_env l tenv = List.fold_left (fun env (x, t) -> Env.add x t env) tenv l
let type_of_unop = function Opp -> TInt | Not -> TBool

let type_of_binop = function
  | Add | Sub | Mul | Div | Rem -> TInt
  | Lt | Le | Gt | Ge | Eq | Neq | And | Or -> TBool

let check_type expected actual =
  if expected <> actual then type_error actual expected

let find_class_def class_name classes =
  try List.find (fun cls -> cls.class_name = class_name) classes
  with Not_found -> error ("Class not found: " ^ class_name)

let find_method_def meth_name methods =
  match List.find_opt (fun m -> m.method_name = meth_name) methods with
  | Some m -> m
  | None -> error ("Method not found: " ^ meth_name)

let objname_of_typ = function TClass clsname -> clsname | _ -> assert false

let typecheck_prog p =
  let tenv : typ Env.t = add_env p.globals Env.empty in
  let find_class_def class_name = find_class_def class_name p.classes in

  let rec check_expr e (tenv: typ Env.t) =
    match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Unop (u, e) -> (
        let type_e = check_expr e tenv in
        match u with
        | Opp ->
            check_type TInt type_e;
            TInt
        | Not ->
            check_type TBool type_e;
            TBool)
    | Binop (u, e1, e2) -> (
        let type_e1 = check_expr e1 tenv in
        let type_e2 = check_expr e2 tenv in
        match u with
        | Lt | Le | Gt | Ge | Eq | Neq ->
            check_type TInt type_e1;
            check_type TInt type_e2;
            TBool
        | Add | Sub | Mul | Div | Rem ->
            check_type TInt type_e1;
            check_type TInt type_e2;
            TInt
        | And | Or ->
            check_type TBool type_e1;
            check_type TBool type_e2;
            TBool)
    | Get m -> type_mem_access m tenv
    | This -> begin 
        
        try 
        (* let c = List.find (fun cls -> cls.class_name = "this") tenv *)
        let c = Env.find  "this" tenv
        in 
        (* TClass (c.class_name) *)
        c
        with Not_found -> error ("Class not found: " ^ "this")
    end

    | New class_name -> TClass class_name
    | NewCstr (class_name, args) ->
        let defclass = find_class_def class_name in
        let constructor = find_method_def "constructor" defclass.methods in
        let param_types = List.map snd constructor.params in
        let arg_types = List.map (fun arg -> check_expr arg tenv) args in
        List.iter2 check_type param_types arg_types;
        TClass class_name
    | MethCall (obj, meth_name, args) ->
        let typcls = objname_of_typ (check_expr obj tenv) in
        let defclass = find_class_def typcls in
        let methodeu = find_method_def meth_name defclass.methods in
        let param_types = List.map snd methodeu.params in
        let arg_types = List.map (fun arg -> check_expr arg tenv) args in
        List.iter2 check_type param_types arg_types;
        methodeu.return
  and type_mem_access m tenv =
    match m with
    | Var name -> (
        match Env.find_opt name tenv with
        | Some typ -> typ
        | None -> error ("Undeclared variable: " ^ name))
    | Field (obj, field_name) -> (
        (* print_endline ("field : " ^ field_name); *)
        let cls_name = objname_of_typ (check_expr obj tenv) in
        (* print_endline ("class : " ^ cls_name); *)
        let class_def = find_class_def cls_name in
        try 
          let r = List.assoc field_name class_def.attributes in 
          r
        with Not_found ->
          error ("Field " ^ field_name ^ " not declared for class " ^ cls_name))
  and check_instr i ret tenv =
    match i with
    | Print e ->
        check_expr e tenv |> check_type TInt
    | If (cond, ifseq, elseseq) ->
        check_expr cond tenv |> check_type TBool;
        check_seq ifseq TVoid tenv;
        check_seq elseseq TVoid tenv
    | While (cond, iseq) ->
        check_expr cond tenv |> check_type TBool;
        check_seq iseq TVoid tenv
    | Set (m, e) ->
        check_expr e tenv |> check_type (type_mem_access m tenv)
    | Return e ->
        let typ_e = check_expr e tenv in
        check_type typ_e ret
    | Expr e -> ignore (check_expr e tenv)
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s in
  check_seq p.main TVoid tenv;

  List.iter (fun c -> 
    let tenv = add_env c.attributes Env.empty in
    let tenv = add_env [("this", TClass c.class_name)] tenv in
    (List.iter (fun m ->
      let tenv = add_env m.locals tenv in
      let tenv = add_env m.params tenv in
      check_seq (m.code) m.return tenv) 
      c.methods)) 
    p.classes

  
