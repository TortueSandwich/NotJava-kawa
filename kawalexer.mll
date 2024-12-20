{
  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "print",    PRINT;
        "main",     MAIN;
        "var",      VAR;
        "if",       IF;
        "else",     ELSE;
        "while",    WHILE;
        "class",    CLASS;
        "extends",  EXTENDS;
        "attribute",ATTRIBUTE;
        "method",   METHOD;
        "new",      NEW;
        "true",     BOOL(true);
        "false",    BOOL(false);
        "this",     THIS;
        "return",   RETURN;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)

  let violet = "\027[35m"
  let jaune = "\027[33m"
  let vert = "\027[92m"
  let orange = "\027[38:5:208m"

  let token_to_string s = 
    let t = match s with
    | PRINT -> violet^"print"
    | MAIN -> "\027[34mmain"
    | VAR -> violet^"var "
    | IDENT(s) -> Printf.sprintf "\027[36mid(%s)" s
    | INT(n) -> Printf.sprintf "\027[96m%d" n
    | BOOL(b:bool) -> Printf.sprintf "\027[96m%b" b
    | SEMI -> jaune^";"
    | LPAR -> jaune^"("
    | RPAR -> jaune^")"
    | BEGIN -> jaune^"{"
    | END -> jaune^"}"
    | COMA -> jaune^","
    | PLUS -> vert^"+"
    | MINUS -> vert^"-"
    | TIMES -> vert^"*"
    | DIV -> vert^"/"
    | EQ -> vert^"=="
    | NEQ -> vert^"!="
    | MOD -> vert^"%"
    | AFFECT -> violet^"="
    | POINT -> "\027[4m."

    | LT -> violet^"<"
    | LEQ -> violet^"<="
    | GT -> violet^">"
    | GEQ -> violet^">="
    | AND -> violet^"&&"
    | OR -> violet^"||"


    | EOF -> "EOF"
    | WHILE -> violet^"while"
    | IF -> violet^"if"
    | ELSE -> violet^"else"
    | CLASS -> violet^"class"
    | EXTENDS -> violet^"extends"
    | RETURN -> violet^"return"
    | NEW -> violet^"new"
    | ATTRIBUTE -> violet^"attribute"
    | METHOD -> violet^"method"
    | THIS -> orange ^"this"
    
    (* | _ -> "UNKNOWN" *)
    in t ^"\027[0m"
    
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

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }

  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIV }
  | "%"  { MOD }
  | "==" { EQ }
  | "=" { AFFECT }
  | "!=" { NEQ }

  | "<" { LT }
  | "<=" { LEQ }
  | ">" { GT }
  | ">=" { GEQ }
  | "&&" { AND }
  | "||" { OR }

  | '.' { POINT }
  | ',' { COMA }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { let tok = EOF in tok }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }