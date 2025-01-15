open Kawa
open Find

type value =
  | Null
  | VInt of int
  | VBool of bool
  | VObj of obj
  | VArray of value array

and obj = { cls : string; fields : (string, value) Hashtbl.t }

(* TODO enleverles erreurs check à la compil *)
type error =
  | InvalidIndex of int * int * string option
  | Division_by_zero of expr
  | TypeCastError of typ * typ

(* | DimensionMismatch of expr *)
(* | NotFound of string *)
(* | NotIndexable of value *)
(* | Anomaly *)

exception IError of error * loc option
exception Return of value

let iraise e = raise (IError (e, None))
let localized_iraise e loc = raise (IError (e, Some loc))

module ValueType = struct
  type t = value

  let string_of_value = function
    | VInt i -> string_of_int i
    | VBool _ -> "BOOL"
    | VObj _ -> "OBJ"
    | VArray _ -> "ARRAY"
    | Null -> "NULL"
end

module Env = Stack_env.MakeEnv (ValueType)

let rec typ_of_value = function
  | VInt _ -> TInt
  | VBool _ -> TBool
  | VObj obj -> TClass (obj.cls, [])
  | VArray v -> TArray (typ_of_value v.(0))
  | Null -> TVoid

let hashtable_values hashtable =
  Hashtbl.fold (fun _ value acc -> value :: acc) hashtable []

let rec init_value = function
  | TInt -> VInt 0
  | TBool -> VBool false
  | TClass _ -> Null
  | TVoid -> Null
  | TArray t -> VArray (Array.make 0 (init_value t))

let rec create_array dims t =
  match dims with
  | [] -> failwith "Dimensions list cannot be empty"
  | [ dim ] ->
      let creator f = VArray (Array.init dim f) in
      let initvalue =
        match t with
        | TInt -> VInt 0
        | TBool -> VBool false
        | TVoid -> Null
        | TClass _ -> Null
        | TArray t ->
            let core_type = Typechecker.elem_type t in
            init_value core_type
      in
      creator (fun _ -> initvalue)
  | dim :: rest -> VArray (Array.init dim (fun _ -> create_array rest t))

let length_varray arr =
  let rec aux a count =
    match a with Array_var (othera, b) -> aux othera (count + 1) | _ -> count
  in
  let arr =
    match arr with Array_var (a, b) -> arr | _ -> failwith "not an array"
  in
  aux arr 0
(* | VArray a ->  *)

let length_varray arr =
  match arr with VArray a -> Array.length a | _ -> failwith "not an array"

let get_elem_from_indices value indexes name =
  List.fold_left
    (fun current idx ->
      match current with
      | VArray arr -> (
          try arr.(idx)
          with _ -> InvalidIndex (idx, Array.length arr, Some name) |> iraise)
      | _ -> failwith "too long dimension")
    value indexes

let set_elem_from_indices value indexes name new_value =
  let rec set_rec current idx_list =
    match (idx_list, current) with
    | [ last_idx ], VArray arr -> (
        try arr.(last_idx) <- new_value
        with _ ->
          InvalidIndex (last_idx, Array.length arr, Some name) |> iraise)
    | idx :: rest, VArray arr -> (
        try set_rec arr.(idx) rest
        with _ -> InvalidIndex (idx, Array.length arr, Some name) |> iraise)
    | _, _ -> failwith "too long dimension"
  in
  set_rec value indexes

let array_of_value v = match v with VArray a -> a | _ -> assert false

let rec ( === ) v1 v2 =
  match (v1, v2) with
  | VInt a, VInt b -> a = b
  | VBool a, VBool b -> a = b
  | Null, Null -> true
  | VObj o1, VObj o2 ->
      assert (o1.cls = o2.cls);
      List.for_all2 ( === )
        (hashtable_values o1.fields)
        (hashtable_values o2.fields)
  | VArray a1, VArray a2 -> Array.for_all2 ( === ) a1 a2
  | _, _ -> failwith "should be typecheck"

let ( =/= ) v1 v2 = not (v1 === v2)

(****** MAIN ATTRACTION ******)
let exec_prog (p : program) : unit =
  let find_class_def = find_class_def p in
  let alloc class_name =
    let c = find_class_def class_name in
    let vartable =
      List.map (fun (name, _, _, _) -> (name, Null)) c.attributes
      |> List.to_seq |> Hashtbl.of_seq
    in
    { cls = class_name; fields = vartable }
  in
  let env_get loc env name = Env.find env name in

  let rec eval_call fname this args =
    let defclass = List.find (fun cls -> cls.class_name = this.cls) p.classes in
    let method_def = find_method_locally_def p defclass fname in
    let method_stack = Env.new_env_stack () in
    let method_env = Env.new_env method_stack in
    (* ajoute arg et this dans l'env de la methode *)
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
      let ( <: ) a b = Find.check_subtype p b a in
      let ( <:? ) a b = if not (a <: b) then TypeCastError (a, b) |> iraise in
      match unop with
      | Opp -> VInt (-evali e env_stack)
      | Not -> VBool (not (evalb e env_stack))
      | TypeCast newType ->
          let v_e = eval e env_stack in
          typ_of_value v_e <:? newType;
          v_e
      | InstanceOf t -> (
          let v_e = eval e env_stack in
          try
            t <:? typ_of_value v_e;
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
          else Division_by_zero e2 |> iraise
      | Rem -> int_op ( mod )
      | Lt -> int_to_bool_op ( < )
      | Le -> int_to_bool_op ( <= )
      | Gt -> int_to_bool_op ( > )
      | Ge -> int_to_bool_op ( >= )
      | Eq -> int_to_bool_op ( = )
      | Neq -> int_to_bool_op ( <> )
      | And -> bool_op ( && )
      | Or -> bool_op ( || )
      | StructEq -> VBool (eval e1 env_stack === eval e2 env_stack)
      | NegStructEq -> VBool (eval e1 env_stack =/= eval e2 env_stack)
    and eval (e : expr) env_stack : value =
      (* print_endline ("evaluation de " ^ string_of_expr e); *)
      let env_get = env_get e.loc in
      match e.expr with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Unop (u, e) -> evalunop u e env_stack
      | Binop (u, e1, e2) -> evalbinop u e1 e2 env_stack
      | Get (Var name) -> env_get env_stack name
      | Get (Field (obj, field_name)) ->
          let o = evalo obj env_stack in
          Hashtbl.find o.fields field_name
      | Get (Array_var (name, index)) ->
          (*todo haha :') (c'est moche)*)
          let memtovar = { annot = e.annot; expr = Get name; loc = e.loc } in
          let v = eval memtovar env_stack in
          let int_idxs = List.map (fun x -> evali x env_stack) index in
          get_elem_from_indices v int_idxs (string_of_mem name)
      | This -> env_get env_stack "this"
      | New (class_name, g) -> VObj (alloc class_name)
      | NewCstr (class_name, gene, args) ->
          let instance = alloc class_name in
          let _ =
            eval_call "constructor" instance
              (List.map (fun x -> eval x env_stack) args)
          in
          VObj instance
      | MethCall (obj, meth_name, args) ->
          let evaled_args = List.map (fun x -> eval x env_stack) args in
          eval_call meth_name (evalo obj env_stack) evaled_args
      | SuperCall (meth_name, args) ->
          let vobj = env_get env_stack "this" in
          let obj = match vobj with VObj o -> o | _ -> assert false in
          let parent = (find_class_def obj.cls).parent in
          let parent_name =
            match parent with Some p -> p | None -> assert false
          in
          let typecasted_obj = { cls = parent_name; fields = obj.fields } in
          eval_call meth_name typecasted_obj
            (List.map (fun x -> eval x env_stack) args)
      | NewArray (t, n) ->
          let idxs = List.map (fun x -> eval x env_stack) n in
          let idx_of_value x =
            match x with
            | VInt n when n > 0 -> n
            | VInt n -> InvalidIndex (n, -1, None) |> iraise (* doit etre > 0*)
            | _ -> failwith "doit etre un int (typechecké)"
          in
          create_array (List.map idx_of_value idxs) t
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
            exec { instr = While (cond, iseq); loc = i.loc } env_stack)
      | Set (m, e) -> (
          match m with
          | Var name -> Env.replace env_stack name (eval e env_stack)
          | Field (obj, field_name) ->
              let o = evalo obj env_stack in
              Hashtbl.replace o.fields field_name (eval e env_stack)
          | Array_var (name, i) ->
              (* toujours moche *)
              let v =
                eval { annot = e.annot; expr = Get name; loc = e.loc } env_stack
              in
              let evaled_index = List.map (fun x -> evali x env_stack) i in
              set_elem_from_indices v evaled_index (string_of_mem name)
                (eval e env_stack))
      | Return e -> raise (Return (eval e env_stack))
      | Expr e -> ignore (eval e env_stack)
      | Scope s ->
          (* exec seq crée deja une nouvelle portée *)
          exec_seq s env_stack
          (* la portée est dropée toute seule*)
      | Declare (s, _, None) ->
          let f x =
            let declared_locally = Env.is_declared_locally env_stack x in
            if not declared_locally then Env.define_locally env_stack x Null
          in
          List.iter f s
      | Declare (s, _, Some v) ->
          let f x =
            let declared_locally = Env.is_declared_locally env_stack x in
            let rvalue = eval v env_stack in
            let action =
              if declared_locally then Env.replace_locally
              else Env.define_locally
            in
            action env_stack x rvalue
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
  List.iter (fun (x, _) -> Env.define_globally x Null) p.globals;
  (* p.globals sont toujours le dernier env d'une stack *)
  let main_env_stack = Env.new_env_stack () in
  ignore (exec_seq p.main main_env_stack)
