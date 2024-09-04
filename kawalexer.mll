{
  open Lexing
  open Kawaparser

  exception Error of string

  let tokens = ref []

  let add_token tok = 
    tokens := tok :: !tokens

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "print",    PRINT;
        "main",     MAIN;
        "var",      VAR;
        "int",      TYPE("int");
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)


  let token_to_string = function
    | PRINT -> "PRINT"
    | MAIN -> "MAIN"
    | VAR -> "VAR"
    | TYPE(t) -> Printf.sprintf "TYPE(%s)" t
    | IDENT(s) -> Printf.sprintf "IDENT(%s)" s
    | SEMI -> "SEMI"
    | LPAR -> "LPAR"
    | RPAR -> "RPAR"
    | BEGIN -> "BEGIN"
    | END -> "END"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | TIMES -> "TIMES"
    | DIV -> "DIV"
    | EQ -> "EQ"
    | EOF -> "EOF"
    | INT(n) -> Printf.sprintf "INT(%d)" n
    | _ -> "UNKNOWN"

  let print_token_list () =
    List.iter (fun tok -> Printf.printf "%s\n" (token_to_string tok)) (List.rev !tokens)
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*

rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { let tok = INT(int_of_string n) in add_token tok; tok }
  | ident as id  { let tok = keyword_or_ident id in add_token tok; tok }

  | ";"  { add_token SEMI; SEMI }
  | "("  { add_token LPAR; LPAR }
  | ")"  { add_token RPAR; RPAR }
  | "{"  { add_token BEGIN; BEGIN }
  | "}"  { add_token END; END }

  | "+"  { add_token PLUS; PLUS }
  | "-"  { add_token MINUS; MINUS }
  | "*"  { add_token TIMES; TIMES }
  | "/"  { add_token DIV; DIV }

  | "=" { add_token EQ; EQ }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { let tok = EOF in add_token tok; tok }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }