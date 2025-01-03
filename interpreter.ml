open Kawa
open Typechecker (*Pour importer check_subtype*)

type value = VInt of int | VBool of bool | VObj of obj | Null
and obj = { cls : string; fields : (string, value) Hashtbl.t }

let typ_of_value = function
  | VInt _ -> TInt
  | VBool _ -> TBool
  | VObj obj -> TClass obj.cls
  | Null -> TVoid

exception Error of string
exception Return of value

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool _ -> "BOOL"
  | VObj _ -> "OBJ"
  | Null -> "NULL"

module ValueType = struct
  type t = value

  let string_of_value = string_of_value
end

module Env = Stack_env.MakeEnv (ValueType)

(* main attraction *)
let exec_prog (p : program) : unit =
  let find_class_def class_name = find_class_def class_name p.classes in
  let check_subtype objective curr =
    check_subtype objective curr find_class_def
  in

  (* let global_env = Env.new_env_stack () in *)
  (* pas de stack a creer *)
  List.iter (fun (x, _) -> Env.define_globally x Null) p.globals;

  let findclass class_name =
    List.find (fun x -> x.class_name = class_name) p.classes
  in
  let alloc class_name =
    let c = findclass class_name in
    let vartable =
      List.map (fun x -> (fst x, Null)) c.attributes
      |> List.to_seq |> Hashtbl.of_seq
    in
    { cls = class_name; fields = vartable }
  in

  let rec eval_call f this args =
    let defclass = List.find (fun cls -> cls.class_name = this.cls) p.classes in
    let rec findmethod defclass =
      match List.find_opt (fun m -> m.method_name = f) defclass.methods with
      | Some m -> m
      | None -> (
          match defclass.parent with
          | Some parent ->
              List.find (fun cls -> cls.class_name = parent) p.classes
              |> findmethod
          | None ->
              raise
                (Error ("Method " ^ f ^ " not found in " ^ defclass.class_name))
          )
    in
    let method_def = findmethod defclass in
    let method_stack = Env.new_env_stack () in
    let method_env = Env.new_env method_stack in
    (* ajoutarg et this dans env *)
    Env.define_locally method_env "this" (VObj this);
    List.iter2
      (fun (param_name, _) arg -> Env.define_locally method_env param_name arg)
      method_def.params args;
    try
      exec_seq method_def.code method_env;
      Null
    with Return v -> v
  and exec_seq s env_stack =
    let rec evali (e : expr) env_stack =
      match eval e env_stack with VInt n -> n | _ -> assert false
    and evalb (e : expr) env_stack =
      match eval e env_stack with VBool b -> b | _ -> assert false
    and evalo (e : expr) env_stack =
      match eval e env_stack with VObj o -> o | _ -> assert false
    and evalunop unop (e : expr) env_stack =
      match unop with
      | Opp -> VInt (-evali e env_stack)
      | Not -> VBool (not (evalb e env_stack))
      | TypeCast newType ->
          (let v_e = eval e env_stack in
          try check_subtype newType (typ_of_value v_e); v_e
          with Typechecker.TypeError s -> (let f = (fst(e.loc)).pos_fname in
                            Typechecker.error (s ^ (Tools.report_bug e.loc f))))
          
      | InstanceOf t -> (
          let v_e = eval e env_stack in
          try
            check_subtype t (typ_of_value v_e);
            VBool true
          with _ -> VBool false)
    and evalbinop binop (e1 : expr) (e2 : expr) env_stack =
      let int_op f = VInt (f (evali e1 env_stack) (evali e2 env_stack)) in
      let bool_op f = VBool (f (evalb e1 env_stack) (evalb e2 env_stack)) in
      let int_to_bool_op f =
        VBool (f (evali e1 env_stack) (evali e2 env_stack))
      in
      match binop with
      | Add -> int_op ( + )
      | Sub -> int_op ( - )
      | Mul -> int_op ( * )
      | Div ->
          let e2_val = evali e2 env_stack in
          if e2_val <> 0 then VInt (evali e1 env_stack / e2_val)
          else failwith "Division by 0"
      | Rem -> int_op ( mod )
      | Lt -> int_to_bool_op ( < )
      | Le -> int_to_bool_op ( <= )
      | Gt -> int_to_bool_op ( > )
      | Ge -> int_to_bool_op ( >= )
      | Eq -> int_to_bool_op ( = )
      | Neq -> int_to_bool_op ( <> )
      | And -> bool_op ( && )
      | Or -> bool_op ( || )
    and eval (e : expr) env_stack : value =
      (* print_endline ("evaluation de " ^ string_of_expr e); *)
      match e.expr with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Unop (u, e) -> evalunop u e env_stack
      | Binop (u, e1, e2) -> evalbinop u e1 e2 env_stack
      | Get (Var name) -> Env.find env_stack name
      | Get (Field (obj, field_name)) ->
          let o = evalo obj env_stack in
          Hashtbl.find o.fields field_name
      | This -> Env.find env_stack "this"
      | New class_name -> VObj (alloc class_name)
      | NewCstr (class_name, args) ->
          let instance = alloc class_name in
          let n =
            eval_call "constructor" instance
              (List.map (fun x -> eval x env_stack) args)
          in
          if n <> Null then
            (* already checked by compiler *)
            raise (Error "A constructor must not return anything")
          else VObj instance
      | MethCall (obj, meth_name, args) ->
          eval_call meth_name (evalo obj env_stack)
            (List.map (fun x -> eval x env_stack) args)
      (* | _ -> failwith "case not implemented in eval" *)
    in

    let rec exec (i : instr) env_stack : unit =
      (* print_endline ("execution de " ^ (string_of_instr i)); *)
      match i.instr with
      | Print e -> Printf.printf "%d\n" (evali e env_stack)
      | If (cond, ifseq, elseseq) ->
          exec_seq (if evalb cond env_stack then ifseq else elseseq) env_stack
      | While (cond, iseq) ->
          if evalb cond env_stack then (
            exec_seq iseq env_stack;
            exec ({instr = While (cond, iseq); loc = i.loc}) env_stack)
      | Set (m, e) -> (
          match m with
          | Var name -> Env.replace env_stack name (eval e env_stack)
          | Field (obj, field_name) ->
              let o = evalo obj env_stack in
              Hashtbl.replace o.fields field_name (eval e env_stack))
      | Return e -> raise (Return (eval e env_stack))
      | Expr e -> ignore (eval e env_stack)
      | Scope s ->
          let env_stack = Env.new_env env_stack in
          exec_seq s env_stack;
          (* pop_env env_stack; *)
          ()
      | Declare (s, _, None) ->
          let f x =
            if Env.is_declared_locally env_stack x then ()
            else Env.define_locally env_stack x Null
          in
          List.iter f s
      | Declare (s, _, Some v) ->
          let f x =
            if Env.is_declared_locally env_stack x then
              Env.replace_locally env_stack x (eval v env_stack)
            else Env.define_locally env_stack x (eval v env_stack)
          in
          List.iter f s
    and exec_seq s env_stack =
      (* nouvelle portée *)
      let env_stack = Env.new_env env_stack in
      List.iter (fun instr -> exec instr env_stack) s;
      let _ = Env.pop_local_env env_stack in
      ()
    in

    exec_seq s env_stack
  in
  let main_env_stack = Env.new_env_stack () in
  ignore (exec_seq p.main main_env_stack)
