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
        "int",      TINT;
        "bool",     TBOOL;
        "void",     TVOID;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)

  let violet = "\027[35m"
  let jaune = "\027[33m"
  let vert = "\027[92m"
  let orange = "\027[38:5:208m"
  let bleu = "\027[94m"

  let token_to_string s = 
    let t = match s with
    | PRINT -> violet^"print"
    | MAIN -> "\027[34mmain"
    | VAR -> violet^"var "
    | IDENT(s) -> Printf.sprintf "\027[36m%s" s
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
    | EXCLAMATION -> vert^"!"
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

    | TINT -> bleu^"int"
    | TBOOL -> bleu^"bool"
    | TVOID -> bleu^"void"
    
    | _ -> "UNKNOWN" 
    in t ^"\027[0m"

  let token_to_string_debug s = 
    let t = match s with
    | PRINT -> "PRINT"
    | MAIN -> "MAIN"
    | VAR -> "VAR"
    | IDENT(s) -> Printf.sprintf "IDENT(%s)" s
    | INT(n) -> Printf.sprintf "IDENT(%d)" n
    | BOOL(b:bool) -> Printf.sprintf "BOOL(%b)" b
    | SEMI -> "SEMI"
    | LPAR -> "LPAR"
    | RPAR -> "RPAR"
    | BEGIN -> "BEGIN"
    | END -> "END"
    | COMA -> "COMA"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | TIMES -> "TIMES"
    | DIV -> "DIV"
    | EQ -> "EQ"
    | NEQ -> "NEQ"
    | MOD -> "MOD"
    | AFFECT -> "AFFECT"
    | POINT -> "POINT"

    | LT -> "LT"
    | LEQ -> "LEQ"
    | GT -> "GT"
    | GEQ -> "GEQ"
    | AND -> "AND"
    | OR -> "OR"

    | EOF -> "EOF"
    | WHILE -> "WHILE"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | CLASS -> "CLASS"
    | EXTENDS -> "EXTENDS"
    | RETURN -> "RETURN"
    | NEW -> "NEW"
    | ATTRIBUTE -> "ATTRIBUTE"
    | METHOD -> "METHOD"
    | THIS -> "THIS"
    
    | _ -> "UNKNOWN"
    in t 
    
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
  | "!" { EXCLAMATION }

  | '.' { POINT }
  | ',' { COMA }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { let tok = EOF in tok }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }