type token =
  | INTEGER of (int)
  | FLOAT of (float)
  | STRING of (string)
  | JOINOP
  | ID of (string)
  | QQUOTE
  | ATTRIBUTE
  | LPAR
  | RPAR
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | PPIPE
  | COMMA
  | DOT
  | ASTERISK
  | SLASH
  | PLUS
  | MINUS
  | CONCAT
  | OR
  | AND
  | NOT
  | IS
  | INNERJOIN
  | JOIN
  | OUTERLEFT
  | OUTERRIGHT
  | OUTERFULL
  | LEFTJOIN
  | RIGHTJOIN
  | FULLJOIN
  | ALL
  | AS
  | BETWEEN
  | BY
  | CROSS
  | DISTINCT
  | FALSE
  | FOR
  | FROM
  | FULL
  | GROUP
  | HAVING
  | INNER
  | LOWER
  | NULL
  | ON
  | OUTER
  | RIGHT
  | LEFT
  | SELECT
  | SUBSTRING
  | TRUE
  | UNKNOWN
  | UPPER
  | WHERE
  | SPACE
  | COMMENTS
  | TERM

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
	open Ast
# 71 "parser.ml"
let yytransl_const = [|
  260 (* JOINOP *);
  262 (* QQUOTE *);
  263 (* ATTRIBUTE *);
  264 (* LPAR *);
  265 (* RPAR *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* LT *);
  269 (* GT *);
  270 (* LE *);
  271 (* GE *);
  272 (* PPIPE *);
  273 (* COMMA *);
  274 (* DOT *);
  275 (* ASTERISK *);
  276 (* SLASH *);
  277 (* PLUS *);
  278 (* MINUS *);
  279 (* CONCAT *);
  280 (* OR *);
  281 (* AND *);
  282 (* NOT *);
  283 (* IS *);
  284 (* INNERJOIN *);
  285 (* JOIN *);
  286 (* OUTERLEFT *);
  287 (* OUTERRIGHT *);
  288 (* OUTERFULL *);
  289 (* LEFTJOIN *);
  290 (* RIGHTJOIN *);
  291 (* FULLJOIN *);
  292 (* ALL *);
  293 (* AS *);
  294 (* BETWEEN *);
  295 (* BY *);
  296 (* CROSS *);
  297 (* DISTINCT *);
  298 (* FALSE *);
  299 (* FOR *);
  300 (* FROM *);
  301 (* FULL *);
  302 (* GROUP *);
  303 (* HAVING *);
  304 (* INNER *);
  305 (* LOWER *);
  306 (* NULL *);
  307 (* ON *);
  308 (* OUTER *);
  309 (* RIGHT *);
  310 (* LEFT *);
  311 (* SELECT *);
  312 (* SUBSTRING *);
  313 (* TRUE *);
  314 (* UNKNOWN *);
  315 (* UPPER *);
  316 (* WHERE *);
  317 (* SPACE *);
  318 (* COMMENTS *);
  319 (* TERM *);
    0|]

let yytransl_block = [|
  257 (* INTEGER *);
  258 (* FLOAT *);
  259 (* STRING *);
  261 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\002\000\002\000\002\000\002\000\
\002\000\002\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\009\000\
\009\000\010\000\010\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\001\000\003\000\003\000\004\000\005\000\001\000\
\002\000\003\000\003\000\003\000\003\000\003\000\004\000\004\000\
\004\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\005\000\006\000\003\000\004\000\004\000\005\000\005\000\006\000\
\007\000\007\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\002\000\001\000\003\000\004\000\004\000\008\000\001\000\
\003\000\001\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\062\000\000\000\044\000\045\000\
\051\000\043\000\060\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\000\001\000\002\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000\000\000\000\000\057\000\
\011\000\000\000\000\000\059\000\000\000\000\000\053\000\000\000\
\054\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\013\000\000\000\010\000\003\000\007\000\006\000\005\000\009\000\
\008\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\021\000\
\020\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\000\000\000\000\055\000\024\000\023\000\
\025\000\000\000\036\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\005\000\006\000\068\000\051\000\084\000\085\000\086\000\019\000\
\020\000\021\000"

let yysindex = "\011\000\
\244\254\000\000\000\255\244\254\000\000\226\254\000\000\000\000\
\000\000\000\000\000\000\078\255\063\255\063\255\030\255\036\255\
\047\255\141\255\019\255\060\255\000\000\000\000\000\000\000\000\
\040\255\042\255\078\255\078\255\078\255\078\255\078\255\078\255\
\078\255\078\255\088\255\032\255\078\255\032\255\032\255\198\255\
\026\255\075\001\220\255\091\255\000\000\056\255\056\255\000\000\
\000\000\048\255\061\255\000\000\097\001\114\001\000\000\078\255\
\000\000\124\255\032\255\000\000\106\255\123\255\130\255\180\255\
\139\255\142\255\013\255\032\255\013\255\013\255\104\255\000\000\
\000\000\032\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\255\013\255\083\255\000\000\169\001\124\001\083\255\
\083\255\078\255\000\000\161\255\109\255\013\255\013\255\031\255\
\078\255\078\255\078\255\078\255\078\255\078\255\137\255\238\254\
\078\255\013\255\178\001\000\000\027\255\109\255\107\255\000\000\
\000\000\000\000\251\255\251\255\251\255\251\255\251\255\251\255\
\078\255\126\255\000\000\189\001\083\255\000\000\000\000\000\000\
\000\000\196\001\000\000\078\255\078\255\251\255\251\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\255\000\000\135\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\129\255\000\000\184\255\239\255\000\000\
\000\000\000\000\251\254\000\000\253\254\254\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\255\000\000\000\000\000\000\008\255\
\011\255\000\000\000\000\000\000\010\001\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\059\001\027\001\000\000\000\000\
\000\000\000\000\070\000\087\000\119\000\136\000\168\000\185\000\
\000\000\000\000\000\000\000\000\076\001\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\217\000\234\000"

let yygindex = "\000\000\
\180\000\137\000\000\000\000\001\022\000\000\000\253\255\118\000\
\000\000\153\000"

let yytablesize = 733
let yytable = "\018\000\
\007\000\008\000\009\000\037\000\010\000\038\000\039\000\122\000\
\024\000\018\000\018\000\001\000\040\000\007\000\008\000\009\000\
\041\000\010\000\011\000\042\000\082\000\012\000\056\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\123\000\
\023\000\018\000\012\000\013\000\049\000\027\000\083\000\050\000\
\014\000\030\000\003\000\028\000\031\000\032\000\033\000\034\000\
\015\000\056\000\004\000\095\000\071\000\096\000\029\000\016\000\
\111\000\037\000\017\000\038\000\039\000\015\000\036\000\007\000\
\008\000\009\000\040\000\010\000\016\000\056\000\041\000\017\000\
\112\000\042\000\031\000\032\000\037\000\059\000\007\000\008\000\
\009\000\011\000\010\000\038\000\012\000\039\000\107\000\113\000\
\114\000\060\000\088\000\089\000\048\000\115\000\116\000\117\000\
\118\000\119\000\120\000\012\000\061\000\124\000\003\000\092\000\
\093\000\062\000\094\000\095\000\063\000\096\000\032\000\015\000\
\064\000\065\000\066\000\109\000\110\000\130\000\016\000\030\000\
\067\000\017\000\031\000\032\000\033\000\034\000\015\000\125\000\
\134\000\135\000\025\000\026\000\072\000\016\000\074\000\096\000\
\017\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\090\000\048\000\127\000\048\000\048\000\075\000\
\048\000\048\000\048\000\048\000\030\000\048\000\076\000\031\000\
\032\000\033\000\034\000\128\000\129\000\048\000\048\000\080\000\
\048\000\108\000\081\000\048\000\048\000\048\000\121\000\131\000\
\048\000\035\000\058\000\048\000\048\000\048\000\048\000\022\000\
\094\000\095\000\058\000\096\000\048\000\052\000\000\000\048\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\000\000\000\000\000\000\046\000\046\000\055\000\046\000\
\046\000\046\000\046\000\000\000\046\000\030\000\000\000\000\000\
\031\000\032\000\033\000\034\000\046\000\046\000\000\000\046\000\
\077\000\000\000\046\000\046\000\046\000\000\000\000\000\046\000\
\078\000\079\000\046\000\046\000\046\000\046\000\031\000\032\000\
\033\000\034\000\000\000\046\000\000\000\000\000\046\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
\000\000\000\000\000\000\047\000\047\000\000\000\047\000\047\000\
\047\000\047\000\030\000\047\000\000\000\031\000\032\000\033\000\
\034\000\000\000\000\000\047\000\047\000\000\000\047\000\000\000\
\000\000\047\000\047\000\047\000\000\000\000\000\047\000\000\000\
\000\000\047\000\047\000\047\000\047\000\053\000\054\000\000\000\
\000\000\000\000\047\000\000\000\000\000\047\000\052\000\052\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\000\000\
\000\000\000\000\073\000\000\000\000\000\052\000\052\000\052\000\
\052\000\000\000\052\000\087\000\000\000\000\000\000\000\000\000\
\000\000\091\000\052\000\052\000\000\000\052\000\027\000\000\000\
\052\000\052\000\052\000\000\000\000\000\052\000\027\000\000\000\
\052\000\052\000\052\000\052\000\000\000\027\000\027\000\028\000\
\027\000\052\000\027\000\000\000\052\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\027\000\028\000\028\000\
\000\000\028\000\027\000\028\000\000\000\027\000\000\000\000\000\
\027\000\027\000\027\000\027\000\000\000\000\000\028\000\029\000\
\000\000\027\000\000\000\028\000\027\000\000\000\028\000\029\000\
\000\000\028\000\028\000\028\000\028\000\000\000\029\000\029\000\
\030\000\029\000\028\000\029\000\000\000\028\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\029\000\030\000\
\030\000\000\000\030\000\029\000\030\000\000\000\029\000\000\000\
\000\000\029\000\029\000\029\000\029\000\000\000\000\000\030\000\
\031\000\000\000\029\000\000\000\030\000\029\000\000\000\030\000\
\031\000\000\000\030\000\030\000\030\000\030\000\000\000\031\000\
\031\000\032\000\031\000\030\000\031\000\000\000\030\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\031\000\
\032\000\032\000\000\000\032\000\031\000\032\000\000\000\031\000\
\000\000\000\000\031\000\031\000\031\000\031\000\000\000\000\000\
\032\000\033\000\000\000\031\000\000\000\032\000\031\000\000\000\
\032\000\033\000\000\000\032\000\032\000\032\000\032\000\000\000\
\033\000\033\000\034\000\033\000\032\000\033\000\000\000\032\000\
\000\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\033\000\034\000\034\000\000\000\034\000\033\000\034\000\000\000\
\033\000\000\000\000\000\033\000\033\000\033\000\033\000\000\000\
\000\000\034\000\017\000\000\000\033\000\000\000\034\000\033\000\
\000\000\034\000\017\000\000\000\034\000\034\000\034\000\034\000\
\000\000\017\000\017\000\019\000\000\000\034\000\017\000\000\000\
\034\000\000\000\000\000\019\000\000\000\000\000\000\000\000\000\
\000\000\017\000\019\000\019\000\000\000\000\000\017\000\019\000\
\000\000\017\000\000\000\000\000\017\000\017\000\017\000\017\000\
\000\000\000\000\019\000\018\000\000\000\017\000\000\000\019\000\
\017\000\000\000\019\000\018\000\000\000\019\000\019\000\019\000\
\019\000\000\000\018\000\057\000\015\000\000\000\019\000\018\000\
\000\000\019\000\030\000\000\000\015\000\031\000\032\000\033\000\
\034\000\000\000\018\000\000\000\000\000\000\000\000\000\018\000\
\015\000\000\000\018\000\000\000\000\000\018\000\018\000\018\000\
\018\000\059\000\000\000\015\000\000\000\000\000\018\000\000\000\
\015\000\018\000\000\000\015\000\000\000\060\000\015\000\015\000\
\015\000\015\000\059\000\000\000\000\000\000\000\000\000\015\000\
\061\000\000\000\015\000\000\000\059\000\062\000\060\000\000\000\
\063\000\000\000\000\000\000\000\064\000\065\000\066\000\000\000\
\060\000\061\000\000\000\000\000\069\000\000\000\062\000\000\000\
\000\000\063\000\000\000\061\000\000\000\064\000\065\000\066\000\
\062\000\000\000\000\000\063\000\000\000\070\000\106\000\064\000\
\065\000\066\000\097\000\098\000\099\000\100\000\101\000\102\000\
\030\000\000\000\126\000\031\000\032\000\033\000\034\000\000\000\
\000\000\030\000\103\000\104\000\031\000\032\000\033\000\034\000\
\000\000\000\000\000\000\000\000\030\000\000\000\105\000\031\000\
\032\000\033\000\034\000\030\000\000\000\132\000\031\000\032\000\
\033\000\034\000\000\000\000\000\133\000"

let yycheck = "\003\000\
\001\001\002\001\003\001\009\001\005\001\009\001\009\001\026\001\
\012\000\013\000\014\000\001\000\009\001\001\001\002\001\003\001\
\009\001\005\001\019\001\009\001\008\001\022\001\017\001\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\050\001\
\063\001\037\000\022\001\036\001\005\001\008\001\026\001\008\001\
\041\001\016\001\055\001\008\001\019\001\020\001\021\001\022\001\
\049\001\044\001\063\001\025\001\056\000\027\001\008\001\056\001\
\026\001\063\001\059\001\063\001\063\001\049\001\044\001\001\001\
\002\001\003\001\063\001\005\001\056\001\044\001\063\001\059\001\
\042\001\063\001\019\001\020\001\017\001\017\001\001\001\002\001\
\003\001\019\001\005\001\044\001\022\001\044\001\090\000\057\001\
\058\001\029\001\069\000\070\000\005\001\097\000\098\000\099\000\
\100\000\101\000\102\000\022\001\040\001\105\000\055\001\082\000\
\083\000\045\001\024\001\025\001\048\001\027\001\020\001\049\001\
\052\001\053\001\054\001\094\000\095\000\121\000\056\001\016\001\
\060\001\059\001\019\001\020\001\021\001\022\001\049\001\106\000\
\132\000\133\000\013\000\014\000\009\001\056\001\029\001\027\001\
\059\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\043\001\019\001\042\001\021\001\022\001\029\001\
\024\001\025\001\026\001\027\001\016\001\029\001\029\001\019\001\
\020\001\021\001\022\001\057\001\058\001\037\001\038\001\029\001\
\040\001\009\001\029\001\043\001\044\001\045\001\038\001\050\001\
\048\001\037\001\044\001\051\001\052\001\053\001\054\001\004\000\
\024\001\025\001\050\000\027\001\060\001\037\000\255\255\063\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\255\255\255\255\021\001\022\001\009\001\024\001\
\025\001\026\001\027\001\255\255\029\001\016\001\255\255\255\255\
\019\001\020\001\021\001\022\001\037\001\038\001\255\255\040\001\
\045\001\255\255\043\001\044\001\045\001\255\255\255\255\048\001\
\053\001\054\001\051\001\052\001\053\001\054\001\019\001\020\001\
\021\001\022\001\255\255\060\001\255\255\255\255\063\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\255\255\255\255\021\001\022\001\255\255\024\001\025\001\
\026\001\027\001\016\001\029\001\255\255\019\001\020\001\021\001\
\022\001\255\255\255\255\037\001\038\001\255\255\040\001\255\255\
\255\255\043\001\044\001\045\001\255\255\255\255\048\001\255\255\
\255\255\051\001\052\001\053\001\054\001\038\000\039\000\255\255\
\255\255\255\255\060\001\255\255\255\255\063\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\255\255\059\000\255\255\255\255\024\001\025\001\026\001\
\027\001\255\255\029\001\068\000\255\255\255\255\255\255\255\255\
\255\255\074\000\037\001\038\001\255\255\040\001\009\001\255\255\
\043\001\044\001\045\001\255\255\255\255\048\001\017\001\255\255\
\051\001\052\001\053\001\054\001\255\255\024\001\025\001\009\001\
\027\001\060\001\029\001\255\255\063\001\255\255\255\255\017\001\
\255\255\255\255\255\255\255\255\255\255\040\001\024\001\025\001\
\255\255\027\001\045\001\029\001\255\255\048\001\255\255\255\255\
\051\001\052\001\053\001\054\001\255\255\255\255\040\001\009\001\
\255\255\060\001\255\255\045\001\063\001\255\255\048\001\017\001\
\255\255\051\001\052\001\053\001\054\001\255\255\024\001\025\001\
\009\001\027\001\060\001\029\001\255\255\063\001\255\255\255\255\
\017\001\255\255\255\255\255\255\255\255\255\255\040\001\024\001\
\025\001\255\255\027\001\045\001\029\001\255\255\048\001\255\255\
\255\255\051\001\052\001\053\001\054\001\255\255\255\255\040\001\
\009\001\255\255\060\001\255\255\045\001\063\001\255\255\048\001\
\017\001\255\255\051\001\052\001\053\001\054\001\255\255\024\001\
\025\001\009\001\027\001\060\001\029\001\255\255\063\001\255\255\
\255\255\017\001\255\255\255\255\255\255\255\255\255\255\040\001\
\024\001\025\001\255\255\027\001\045\001\029\001\255\255\048\001\
\255\255\255\255\051\001\052\001\053\001\054\001\255\255\255\255\
\040\001\009\001\255\255\060\001\255\255\045\001\063\001\255\255\
\048\001\017\001\255\255\051\001\052\001\053\001\054\001\255\255\
\024\001\025\001\009\001\027\001\060\001\029\001\255\255\063\001\
\255\255\255\255\017\001\255\255\255\255\255\255\255\255\255\255\
\040\001\024\001\025\001\255\255\027\001\045\001\029\001\255\255\
\048\001\255\255\255\255\051\001\052\001\053\001\054\001\255\255\
\255\255\040\001\009\001\255\255\060\001\255\255\045\001\063\001\
\255\255\048\001\017\001\255\255\051\001\052\001\053\001\054\001\
\255\255\024\001\025\001\009\001\255\255\060\001\029\001\255\255\
\063\001\255\255\255\255\017\001\255\255\255\255\255\255\255\255\
\255\255\040\001\024\001\025\001\255\255\255\255\045\001\029\001\
\255\255\048\001\255\255\255\255\051\001\052\001\053\001\054\001\
\255\255\255\255\040\001\009\001\255\255\060\001\255\255\045\001\
\063\001\255\255\048\001\017\001\255\255\051\001\052\001\053\001\
\054\001\255\255\024\001\009\001\009\001\255\255\060\001\029\001\
\255\255\063\001\016\001\255\255\017\001\019\001\020\001\021\001\
\022\001\255\255\040\001\255\255\255\255\255\255\255\255\045\001\
\029\001\255\255\048\001\255\255\255\255\051\001\052\001\053\001\
\054\001\017\001\255\255\040\001\255\255\255\255\060\001\255\255\
\045\001\063\001\255\255\048\001\255\255\029\001\051\001\052\001\
\053\001\054\001\017\001\255\255\255\255\255\255\255\255\060\001\
\040\001\255\255\063\001\255\255\017\001\045\001\029\001\255\255\
\048\001\255\255\255\255\255\255\052\001\053\001\054\001\255\255\
\029\001\040\001\255\255\255\255\060\001\255\255\045\001\255\255\
\255\255\048\001\255\255\040\001\255\255\052\001\053\001\054\001\
\045\001\255\255\255\255\048\001\255\255\060\001\051\001\052\001\
\053\001\054\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\009\001\019\001\020\001\021\001\022\001\255\255\
\255\255\016\001\026\001\027\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\016\001\255\255\038\001\019\001\
\020\001\021\001\022\001\016\001\255\255\025\001\019\001\020\001\
\021\001\022\001\255\255\255\255\025\001"

let yynames_const = "\
  JOINOP\000\
  QQUOTE\000\
  ATTRIBUTE\000\
  LPAR\000\
  RPAR\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  PPIPE\000\
  COMMA\000\
  DOT\000\
  ASTERISK\000\
  SLASH\000\
  PLUS\000\
  MINUS\000\
  CONCAT\000\
  OR\000\
  AND\000\
  NOT\000\
  IS\000\
  INNERJOIN\000\
  JOIN\000\
  OUTERLEFT\000\
  OUTERRIGHT\000\
  OUTERFULL\000\
  LEFTJOIN\000\
  RIGHTJOIN\000\
  FULLJOIN\000\
  ALL\000\
  AS\000\
  BETWEEN\000\
  BY\000\
  CROSS\000\
  DISTINCT\000\
  FALSE\000\
  FOR\000\
  FROM\000\
  FULL\000\
  GROUP\000\
  HAVING\000\
  INNER\000\
  LOWER\000\
  NULL\000\
  ON\000\
  OUTER\000\
  RIGHT\000\
  LEFT\000\
  SELECT\000\
  SUBSTRING\000\
  TRUE\000\
  UNKNOWN\000\
  UPPER\000\
  WHERE\000\
  SPACE\000\
  COMMENTS\000\
  TERM\000\
  "

let yynames_block = "\
  INTEGER\000\
  FLOAT\000\
  STRING\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.query) in
    Obj.repr(
# 60 "parser.mly"
                                       ( _2 )
# 491 "parser.ml"
               : Ast.query))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'query) in
    Obj.repr(
# 61 "parser.mly"
                                       ( _1 )
# 498 "parser.ml"
               : Ast.query))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                             ( innerJoin )
# 504 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                         ( join )
# 510 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                            ( outerleft )
# 516 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                             ( outerright )
# 522 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                            ( outerfull )
# 528 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                            ( left )
# 534 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                            ( right )
# 540 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                            ( full )
# 546 "parser.ml"
               : 'joinop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                      ( idS _1 )
# 553 "parser.ml"
               : 'source))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'query) in
    Obj.repr(
# 75 "parser.mly"
                                ( parS _2 )
# 560 "parser.ml"
               : 'source))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'source) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'source) in
    Obj.repr(
# 76 "parser.mly"
                                   ( croissJoin _1 _3 )
# 568 "parser.ml"
               : 'source))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'source) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'source) in
    Obj.repr(
# 77 "parser.mly"
                                       ( croissJoin _1 _4 )
# 576 "parser.ml"
               : 'source))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'source) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'joinop) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'source) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 78 "parser.mly"
                                              ( joinNop _1 _2 _3 _5)
# 586 "parser.ml"
               : 'source))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 82 "parser.mly"
                             ( predc _1 )
# 593 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 83 "parser.mly"
                                ( notC _2 )
# 600 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 84 "parser.mly"
                                      ( orC _1 _3)
# 608 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 85 "parser.mly"
                                       ( andC _1 _3)
# 616 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    Obj.repr(
# 86 "parser.mly"
                                   ( isTrue _1)
# 623 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    Obj.repr(
# 87 "parser.mly"
                                   ( isFalse _1)
# 630 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    Obj.repr(
# 88 "parser.mly"
                                     ( isUnknown _1)
# 637 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    Obj.repr(
# 89 "parser.mly"
                                      ( isNotTrue _1)
# 644 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    Obj.repr(
# 90 "parser.mly"
                                      ( isNotFalse _1)
# 651 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    Obj.repr(
# 91 "parser.mly"
                                        ( isNotUnknown _1)
# 658 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'condition) in
    Obj.repr(
# 95 "parser.mly"
                                   ( parP _2 )
# 665 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 96 "parser.mly"
                                       ( eq _1 _3 )
# 673 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 97 "parser.mly"
                                        ( neq _1 _3 )
# 681 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 98 "parser.mly"
                                       ( lt _1 _3 )
# 689 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 99 "parser.mly"
                                       ( gt _1 _3 )
# 697 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 100 "parser.mly"
                                       ( le _1 _3 )
# 705 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 101 "parser.mly"
                                       ( ge _1 _3 )
# 713 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 102 "parser.mly"
                                                      ( btwA _1 _3 _5 )
# 722 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 103 "parser.mly"
                                                          ( notBtwA _1 _4 _6)
# 731 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 104 "parser.mly"
                                   ( isNull _1 )
# 738 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    Obj.repr(
# 105 "parser.mly"
                                      ( isNotNull _1 )
# 745 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'projection) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'source) in
    Obj.repr(
# 108 "parser.mly"
                                             ( sf _2 _4 )
# 753 "parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'projection) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'source) in
    Obj.repr(
# 109 "parser.mly"
                                              ( saf _3 _5 )
# 761 "parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'projection) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'source) in
    Obj.repr(
# 110 "parser.mly"
                                                 ( sdf _3 _5 )
# 769 "parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'projection) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'source) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 111 "parser.mly"
                                                       ( sfw _2 _4 _6 )
# 778 "parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'projection) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'source) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 112 "parser.mly"
                                                          ( safw _3 _5 _7 )
# 787 "parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'projection) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'source) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 113 "parser.mly"
                                                             ( sdfw _3 _5 _7 )
# 796 "parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                       ( attribut _1 )
# 803 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 118 "parser.mly"
                             ( integer _1 )
# 810 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 119 "parser.mly"
                          ( valueFloat _1 )
# 817 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 120 "parser.mly"
                                            ( plus _1 _3 )
# 825 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 121 "parser.mly"
                                            ( minus _1 _3 )
# 833 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 122 "parser.mly"
                                             ( mult _1 _3 )
# 841 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 123 "parser.mly"
                                          ( div _1 _3 )
# 849 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 124 "parser.mly"
                                             ( minusN _2 )
# 856 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "parser.mly"
                          ( chaine _1 )
# 863 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 126 "parser.mly"
                                             ( string_of_ppipe _1 _3 )
# 871 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 127 "parser.mly"
                                         ( lower _3 )
# 878 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 128 "parser.mly"
                                         ( upper _3 )
# 885 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 129 "parser.mly"
                                                                   ( sous_chaine _3 _5 _7 )
# 894 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 133 "parser.mly"
                            ( cexpr _1 )
# 901 "parser.ml"
               : 'column))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
                                 ( cexprAs _1 _3 )
# 909 "parser.ml"
               : 'column))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'column) in
    Obj.repr(
# 138 "parser.mly"
                         ( ccol _1)
# 916 "parser.ml"
               : 'columns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'column) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'columns) in
    Obj.repr(
# 139 "parser.mly"
                                                ( ccolplus _1 _3 )
# 924 "parser.ml"
               : 'columns))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                            ( asterisk )
# 930 "parser.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'columns) in
    Obj.repr(
# 144 "parser.mly"
                          ( projColumn _1 )
# 937 "parser.ml"
               : 'projection))
(* Entry ansyn *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let ansyn (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.query)
