%{
	open Ast
%}

/*Declaration des terminaux */

%token<int> INTEGER
%token<float> FLOAT
%token<string> STRING
%token JOINOP
%token <string> ID
%token QQUOTE
%token ATTRIBUTE
%token LPAR RPAR
%token EQ NEQ LT GT LE GE 
%token QQUOTE PPIPE COMMA DOT
%token ASTERISK SLASH PLUS MINUS
%token CONCAT
%token OR AND NOT IS   /*Dans le keyword */
%token INNERJOIN JOIN OUTERLEFT OUTERRIGHT OUTERFULL LEFTJOIN RIGHTJOIN FULLJOIN
%token ALL AS BETWEEN BY CROSS DISTINCT FALSE FOR FROM FULL 
%token GROUP HAVING INNER JOIN LOWER  NULL ON OUTER RIGHT LEFT SELECT
%token SUBSTRING TRUE UNKNOWN UPPER WHERE
%token SPACE COMMENTS 
%token INNERJOIN OUTERLEFT OUTERRIGHT OUTERFULL LEFTJOIN RIGHTJOIN FULLJOIN
%token TERM

/*TODO  verifie les priorite des keyword*/
%nonassoc FROM
%nonassoc WHERE NULL SELECT 

%nonassoc JOINOP
%nonassoc ON
%left PROJCOMMA
%left COMMA JOIN CROSS LEFT RIGHT INNER OUTER FULL 
%nonassoc EQ NEQ LT GT LE GE
/* Précédences (priorité + associativité) des terminaux */
%left  PPIPE
%left PLUS MINUS
%left ASTERISK 
%nonassoc SLASH 
%left CONCAT
%left OR 
%left AND 
%left NOT 
%nonassoc UMINUS
%left IS


/* Déclaration du non-terminal axiome (ici, ansyn) et du type de son attribut */
%type <Ast.query> ansyn
%start ansyn

%%

/* Déclaration de la grammaire avec les actions sémantiques */
/*recupere le nom de la fonction dans AST*/

ansyn:
	| TERM ansyn              												{ $2 }
	| query TERM              												{ $1 } 
;
joinop :
	| INNER JOIN 															{ innerJoin }
	| JOIN 																	{ join }
	| OUTER LEFT															{ outerleft }
	| OUTER RIGHT															{ outerright }
	| OUTER FULL															{ outerfull }
	| LEFT JOIN																{ left }
	| RIGHT JOIN															{ right }
	| FULL JOIN																{ full }
	;
source : 
	| ID																	{ idS $1 }
	| LPAR query RPAR														{ parS $2 }
	| source COMMA source													{ croissJoin $1 $3 }
	| source CROSS JOIN source												{ croissJoin $1 $4 }
	| source joinop source ON condition										{ joinNop $1 $2 $3 $5}	

;
condition :
 	| predicate 															{ predc $1 }
 	| NOT condition															{ notC $2 }
 	| condition OR condition												{ orC $1 $3}
 	| condition AND condition												{ andC $1 $3}	
 	| condition IS TRUE 													{ isTrue $1}
 	| condition IS FALSE													{ isFalse $1}
 	| condition IS UNKNOWN													{ isUnknown $1}
 	| condition IS NOT TRUE 												{ isNotTrue $1}
 	| condition IS NOT FALSE												{ isNotFalse $1}
 	| condition IS NOT UNKNOWN												{ isNotUnknown $1}											
;	

predicate :	
	| LPAR condition RPAR													{ parP $2 }
	| expression EQ expression												{ eq $1 $3 }
	| expression NEQ expression												{ neq $1 $3 }
	| expression LT expression												{ lt $1 $3 }
	| expression GT expression												{ gt $1 $3 }
	| expression LE expression												{ le $1 $3 }
	| expression GE expression												{ ge $1 $3 }
	| expression BETWEEN expression AND expression							{ btwA $1 $3 $5 }
	| expression NOT BETWEEN expression	 AND expression						{ notBtwA $1 $4 $6}
	| expression IS NULL 													{ isNull $1 }
	| expression IS NOT NULL 												{ isNotNull $1 }
;
query:
  	| SELECT projection FROM source											{ sf $2 $4 }
	| SELECT ALL projection FROM source 									{ saf $3 $5 }
	| SELECT DISTINCT projection FROM source								{ sdf $3 $5 }
	| SELECT projection FROM source WHERE condition							{ sfw $2 $4 $6 }	
	| SELECT ALL projection FROM source WHERE condition						{ safw $3 $5 $7 }
	| SELECT DISTINCT projection FROM source WHERE condition				{ sdfw $3 $5 $7 }
;

expression :
	| ID 																	{ attribut $1 }
	| INTEGER							   									{ integer $1 } 
	| FLOAT 							 									{ valueFloat $1 } 
	| expression PLUS expression     										{ plus $1 $3 }
	| expression MINUS expression    										{ minus $1 $3 }
	| expression ASTERISK expression  										{ mult $1 $3 }	
	| expression SLASH expression 											{ div $1 $3 }
	| MINUS expression %prec UMINUS   										{ minusN $2 }
	| STRING 																{ chaine $1 }
	| expression PPIPE expression 										    { string_of_ppipe $1 $3 }	
	| LOWER LPAR expression RPAR	 										{ lower $3 }
	| UPPER LPAR expression RPAR	 										{ upper $3 }
	| SUBSTRING LPAR expression FROM expression FOR expression RPAR 		{ sous_chaine $3 $5 $7 }
;

column : 
	| expression															{ cexpr $1 }
	| expression AS ID														{ cexprAs $1 $3 }
;

columns :
	| column																{ ccol $1}
	| column COMMA columns				%prec PROJCOMMA						{ ccolplus $1 $3 }
;

projection : 
	| ASTERISK 																{ asterisk }
	| columns																{ projColumn $1 }
;




