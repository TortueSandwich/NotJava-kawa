open Kawa

type value = VInt of int | VBool of bool | VObj of obj | Null
and obj = { cls : string; fields : (string, value) Hashtbl.t }

exception Error of string
exception Return of value

let rec string_of_value = function
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

let exec_prog (p : program) : unit =
  let env_stack = [ Hashtbl.create 16 ] in
  List.iter (fun (x, _) -> Hashtbl.add (List.hd env_stack) x Null) p.globals;

  let rec eval_call f this args =
    let defclass = List.find (fun cls -> cls.class_name = this.cls) p.classes in
    let rec findmethod defclass =
      match List.find_opt (fun m -> m.method_name = f) defclass.methods with
      | Some m -> m
      | None -> (
          match defclass.parent with
          | Some parent ->
              let defclass =
                List.find (fun cls -> cls.class_name = parent) p.classes
              in
              findmethod defclass
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
    and evalunop unop (e : expr) =
      match unop with Opp -> VInt (-evali e) | Not -> VBool (not (evalb e)) | TypeCast (newType) -> 
                                                                              match newType with TClass (newClassName) -> match eval e with VObj obj -> VObj ({ cls=newClassName; fields=obj.fields})
    and evalbinop binop (e1 : expr) (e2 : expr) =
      let int_op f = VInt (f (evali e1) (evali e2)) in
      let bool_op f = VBool (f (evalb e1) (evalb e2)) in
      match binop with
      | Add -> int_op ( + )
      | Sub -> int_op ( - )
      | Mul -> int_op ( * )
      | Div ->
          let e2_val = evali e2 in
          if e2_val <> 0 then VInt (evali e1 / e2_val)
          else failwith "Division by 0"
      | Rem -> int_op (mod)
      | Lt ->
          let v1 = evali e1 in
          let v2 = evali e2 in
          VBool (v1 < v2)
      | Le ->
          let v1 = evali e1 in
          let v2 = evali e2 in
          VBool (v1 <= v2)
      | Gt ->
          let v1 = evali e1 in
          let v2 = evali e2 in
          VBool (v1 > v2)
      | Ge ->
          let v1 = evali e1 in
          let v2 = evali e2 in
          VBool (v1 >= v2)
      | Eq -> VBool (eval e1 = eval e2)
      | Neq -> VBool (eval e1 <> eval e2)
      | And ->
          let v1 = evalb e1 in
          let v2 = evalb e2 in
          VBool (v1 && v2)
      | Or ->
          let v1 = evalb e1 in
          let v2 = evalb e2 in
          VBool (v1 || v2)
    and eval (e : expr) : value =
      match e with
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
      | New class_name ->
          let defclass =
            List.find (fun x -> x.class_name = class_name) p.classes
          in
          let vartable = Hashtbl.create (List.length defclass.attributes) in
          List.iter
            (fun (varname, _) -> Hashtbl.add vartable varname Null)
            defclass.attributes;
          let classetqt : obj = { cls = class_name; fields = vartable } in
          VObj classetqt
      | NewCstr (class_name, args) ->
          let defclass =
            List.find (fun x -> x.class_name = class_name) p.classes
          in
          let vartable = Hashtbl.create (List.length defclass.attributes) in
          List.iter
            (fun (varname, _) -> Hashtbl.add vartable varname Null)
            defclass.attributes;

          let classetqt : obj = { cls = class_name; fields = vartable } in
          eval_call "constructor" classetqt (List.map eval args);
          VObj classetqt
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
    and exec_seq s env_stack =
      let env_stack = push_env env_stack in
      List.iter (fun instr -> exec instr) s;
      let _ = pop_env env_stack in
      ()
    in

    exec_seq s env_stack
  in

  ignore (exec_seq p.main [])
