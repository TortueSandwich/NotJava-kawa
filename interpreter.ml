open Kawa
open Typechecker  (*Pour importer check_subtype*)

type value = VInt of int | VBool of bool | VObj of obj | Null
and obj = { cls : string; fields : (string, value) Hashtbl.t }


let typ_of_value = function
| VInt (_) -> TInt
| VBool (_) -> TBool
| VObj (obj)-> TClass (obj.cls)
| Null -> TVoid 

exception Error of string
exception Return of value

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool _ -> "BOOL"
  | VObj _ -> "OBJ"
  | Null -> "NULL"

let print_hashtable table =
  Hashtbl.iter
    (fun key value ->
      Printf.printf "Key: %s, Value: %s\n" key (string_of_value value))
    table;
  print_endline "--"

(* Gestion des portées avec une pile d'environnements *)
let push_env env_stack =
  let new_env = Hashtbl.create 16 in
  new_env :: env_stack

let pop_env = function
  | [] -> failwith "Impossible de dépiler : aucun environnement"
  | _ :: env_stack -> env_stack

let rec find_in_env env_stack key =
  match env_stack with
  | [] -> raise (Error ("Variable non définie: " ^ key))
  | env :: rest ->
      if Hashtbl.mem env key then Hashtbl.find env key else find_in_env rest key

let set_in_env env_stack key value =
  match env_stack with
  | [] -> raise (Error ("Impossible de définir la variable: " ^ key))
  | env :: _ -> Hashtbl.replace env key value (* print_hashtable env; *)

(* main attraction *)
let exec_prog (p : program) : unit =
  let find_class_def class_name = find_class_def class_name p.classes in
  let check_subtype objective curr = check_subtype objective curr find_class_def in
  let env_stack = [ Hashtbl.create 16 ] in
  List.iter (fun (x, _) -> Hashtbl.add (List.hd env_stack) x Null) p.globals;
  let findclass class_name = List.find (fun x -> x.class_name = class_name) p.classes in
  let alloc class_name = 
    let c = findclass class_name in
    let vartable = List.map (fun x -> (fst x,Null)) c.attributes |> List.to_seq |> Hashtbl.of_seq in
    { cls = class_name; fields = vartable }
  in

  let rec eval_call f this args =
    let defclass = List.find (fun cls -> cls.class_name = this.cls) p.classes in
    let rec findmethod defclass =
      match List.find_opt (fun m -> m.method_name = f) defclass.methods with
      | Some m -> m
      | None -> (
          match defclass.parent with
          | Some parent -> List.find (fun cls -> cls.class_name = parent) p.classes |> findmethod
          | None ->
              raise
                (Error ("Method " ^ f ^ " not found in " ^ defclass.class_name))
          )
    in
    let method_def = findmethod defclass in
    let method_env = Hashtbl.create 16 in
    Hashtbl.add method_env "this" (VObj this);
    List.iter2
      (fun (param_name, _) arg -> Hashtbl.add method_env param_name arg)
      method_def.params args;
    try
      exec_seq method_def.code [ method_env ];
      Null
    with Return v -> v
  and exec_seq s (env_stack : (string, value) Hashtbl.t list) =
    let env_stack = push_env env_stack in
    let rec evali (e : expr) =
      match eval e with VInt n -> n | _ -> assert false
    and evalb (e : expr) = match eval e with VBool b -> b | _ -> assert false
    and evalo (e : expr) = match eval e with VObj o -> o | _ -> assert false
    and evalunop unop (e : expr) = match unop with 
      | Opp -> VInt (-evali e) 
      | Not -> VBool (not (evalb e)) 
      | TypeCast (newType) -> (
        let v_e = eval e in
        check_subtype newType (typ_of_value v_e);
        v_e)
      | InstanceOf (t) -> (
        let v_e = eval e in 
        try 
          check_subtype t (typ_of_value v_e); VBool (true)
        with
         _ -> VBool (false)
      )
    and evalbinop binop (e1 : expr) (e2 : expr) =
      let int_op f = VInt (f (evali e1) (evali e2)) in
      let bool_op f = VBool (f (evalb e1) (evalb e2)) in
      let int_to_bool_op f = VBool (f (evali e1) (evali e2)) in
      match binop with
      | Add -> int_op ( + )
      | Sub -> int_op ( - )
      | Mul -> int_op ( * )
      | Div ->
          let e2_val = evali e2 in
          if e2_val <> 0 then VInt (evali e1 / e2_val)
          else failwith "Division by 0"
      | Rem -> int_op (mod)
      | Lt -> int_to_bool_op ( < )
      | Le -> int_to_bool_op ( <= )
      | Gt -> int_to_bool_op ( > )
      | Ge -> int_to_bool_op ( >= )
      | Eq -> int_to_bool_op ( = )
      | Neq -> int_to_bool_op ( <> )
      | And -> bool_op ( && )
      | Or -> bool_op ( || )
    and eval (e : expr) : value =
      match e.expr with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Unop (u, e) -> evalunop u e
      | Binop (u, e1, e2) -> evalbinop u e1 e2
      | Get m -> (
          match m with
          | Var name -> find_in_env env_stack name
          | Field (obj, field_name) ->
              let o = evalo obj in
              Hashtbl.find o.fields field_name)
      | This -> find_in_env env_stack "this"
      | New class_name -> VObj(alloc class_name)
      | NewCstr (class_name, args) ->
          let instance = alloc class_name in
          let n = eval_call "constructor" instance (List.map eval args) in
          if n <> Null then raise (Error("A constructor must not return anything")) (* already checked by compiler *)
          else VObj instance
      | MethCall (obj, meth_name, args) ->
          eval_call meth_name (evalo obj) (List.map eval args)
      (* | _ -> failwith "case not implemented in eval" *)
    in

    let rec exec (i : instr) : unit =
      match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | If (cond, ifseq, elseseq) ->
          exec_seq (if evalb cond then ifseq else elseseq) env_stack
      | While (cond, iseq) ->
          if evalb cond then (
            exec_seq iseq env_stack;
            exec (While (cond, iseq)))
      | Set (m, e) -> (
          match m with
          | Var name -> set_in_env env_stack name (eval e)
          | Field (obj, field_name) ->
              let o = evalo obj in
              Hashtbl.replace o.fields field_name (eval e))
      | Return e -> raise (Return (eval e))
      | Expr e -> ignore (eval e)
      | Scope s -> exec_seq s env_stack
    and exec_seq s env_stack =
      let env_stack = push_env env_stack in
      List.iter (fun instr -> exec instr) s;
      let _ = pop_env env_stack in
      ()
    in

    exec_seq s env_stack
  in

  ignore (exec_seq p.main [])
