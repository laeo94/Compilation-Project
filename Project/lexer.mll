{
open Parser
exception Eof
}

let alpha = ['a'-'z' 'A'-'Z']
let digit =['0'-'9']
let id =(alpha(digit|alpha)*) |('\"' + (alpha|digit|''')+ '\"')

let integer = digit+
let attribute = id"."id
let float = (integer"."integer?(('e'|'E')('-'|'+')? integer)?)
            |'.'integer(('e'|'E')('-'|'+')? integer)?

let joinop = ("INNER"?|(("LEFT"|"RIGHT"|"FULL")"OUTER"?))"JOIN"


rule token = parse
  | [' ' '\t' '\n' '\r']                  { token lexbuf (* Oubli des espacements et passages à la ligne *) }
  | "*"                                   { ASTERISK }
  | "\""                                  { QQUOTE }
  | attribute                             { ATTRIBUTE }
  | '.'                                   { DOT }
  | '('                                   { LPAR }
  | '('                                   { RPAR }
  | integer  as integ                     { INTEGER (int_of_string integ) }
  | float    as fl                        { FLOAT (float_of_string fl) }
  | '+'                                   { PLUS }
  | '-'                                   { MINUS }
  | '/'                                   { SLASH }
  | '''                                   { QQUOTE }
  | "||"                                  { PPIPE }
  | ','                                   { COMMA }
  | joinop                                { JOINOP }
  | "="                                   { EQ }
  | "<>"                                  { NEQ }
  | "<"                                   { LT }
  | ">"                                   { GT }
  | "<="                                  { LE }
  | ">="                                  { GE }
  | "--"                                  { comment lexbuf }
  | "ALL"                                 { ALL }
  | "AND"                                 { AND }
  | "AS"                                  { AS }
  | "BETWEEN"                             { BETWEEN }
  | "BY"                                  { BY }
  | "CROSS"                               { CROSS }
  | "DISTINCT"                            { DISTINCT }
  | "FALSE"                               { FALSE }
  | "FOR"                                 { FOR }
  | "FROM"                                { FROM }
  | "FULL"                                { FULL }
  | "GROUP"                               { GROUP }
  | "HAVING"                              { HAVING }
  | "INNER"                               { INNER }
  | "IS"                                  { IS }
  | "JOIN"                                { JOIN }
  | "LEFT"                                { LEFT }
  | "LOWER"                               { LOWER }
  | "NOT"                                 { NOT }
  | "NULL"                                { NULL }
  | "ON"                                  { ON }
  | "OR"                                  { OR }
  | "OUTER"                               { OUTER }
  | "RIGHT"                               { RIGHT }
  | "SELECT"                              { SELECT }
  | "SUBSTRING"                           { SUBSTRING }
  | "TRUE"                                { TRUE }
  | "UNKNOWN"                             { UNKNOWN }
  | "UPPER"                               { UPPER }
  | "WHERE"                               { WHERE }
  | '''((alpha|digit)|'''''')*''' as str  { STRING str}
  | id                                    { ID (Lexing.lexeme lexbuf) }
  | ';'                                   { TERM }  
  | eof                                   { raise Eof }
  | _  as lxm                             { (* Pour tout autre caractère : message sur la sortie erreur + oubli *)
                                           Printf.eprintf "Unknown character '%c': ignored\n" lxm; flush stderr;
                                                token lexbuf
                                          }
and comment = parse
  | "\n"                                  { token lexbuf }
  |  _[^'\"' ]                            { comment lexbuf }
  | eof                                   { raise Eof }
