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
        | Div, VInt a, VInt b -> 
          if b = 0 then failwith "Division by zero :("
          else VInt(a / b)
        | _ -> failwith (Printf.sprintf "case not implemented in eval : cant stand operation")
        end
      | Get(Var (x)) -> 
          (match Hashtbl.find_opt env x with
           | Some v -> v
           | None -> failwith (Printf.sprintf "undefined variable: %s" x))
      | _ -> failwith (Printf.sprintf "case not implemented in eval : %S" (Kawa.string_of_expr e))
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | Set(mem, var) -> 
        let var = eval var in
        match mem with 
        | Var x -> Hashtbl.replace env x var
      | _ -> failwith "case not implemented in exec"
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  let hstbl = Hashtbl.create 1 in
  List.iter (fun (n, _) -> Hashtbl.add hstbl n Null) p.globals;
  
  exec_seq p.main (hstbl)
