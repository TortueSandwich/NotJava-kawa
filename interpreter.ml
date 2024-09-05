open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}
let string_of_value v = match v with
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  (* | VObj o *)
  | Null -> "!NULL!"

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args =
    failwith "eval_call not implemented"

  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | VInt n -> n <> 0
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Binop(op, e1, e2) -> begin
        let (v1, v2) = (eval e1, eval e2) in
        match (op, v1, v2) with 
        | Add, VInt a, VInt b -> VInt(a + b)
        | Sub, VInt a, VInt b -> VInt(a - b)
        | Mul, VInt a, VInt b -> VInt(a * b)
        | Rem, VInt a, VInt b -> VInt(a mod b)
        | Div, VInt a, VInt b -> 
          if b = 0 then failwith "Division by zero :("
          else VInt(a / b)
        | Neq, a, b -> VBool((evalb e1) != (evalb e2))
        | _ -> failwith (Printf.sprintf "case not implemented in eval : cant stand operation %s on %s & %s" (string_of_biop op) (string_of_value v1) (string_of_value v2))
        end
      | Get(Var (x)) -> 
          (match Hashtbl.find_opt env x with
           | Some v -> v
           | None -> failwith (Printf.sprintf "undefined variable: %s" x))
      | _ -> failwith (Printf.sprintf "case not implemented in eval : %S" (Kawa.string_of_expr e))
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | Set(mem, var) -> begin
        let var = eval var in
        match mem with 
          | Var x -> Hashtbl.replace env x var
        end
      | While(c,d) -> begin
          let rec aux () = if evalb c then (exec_seq d; aux ()) in
          aux ()
        end
      | If(c, a, b) -> if evalb c then exec_seq a else exec_seq b
      | _ -> failwith "case not implemented in exec"
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  let hstbl = Hashtbl.create 1 in
  List.iter (fun (n, _) -> Hashtbl.add hstbl n Null) p.globals;
  
  exec_seq p.main (hstbl)
