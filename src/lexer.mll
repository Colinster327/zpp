{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'?digit+
let float = '-'?digit+'.'digit+
let letter = ['a'-'z' 'A'-'Z']
let str = '"' [^ '"']* '"'
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | str { STRING (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | "()" { UNIT }
  | "fr" { TRUE }
  | "nah" { FALSE }
  | "!" { EXCLAM }
  | "<=" { LEQ }
  | "<" { LT }
  | ">" { GT }
  | ">=" { GEQ }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { SLASH }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACK }
  | "}" { RBRACK }
  | "slay" { COUT }
  | "vibecheck" { LET }
  | "lowkey" { IF }
  | "cap" { ELSE }
  | ";" { SEMI }
  | eof { EOF }
