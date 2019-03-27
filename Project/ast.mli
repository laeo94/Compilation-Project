module R : Relation.S
(* Syntaxe abstraite *)
type query
type column
type columns
type projection
type expression
type source
type joinNop
type condition
type predicate

(* Constructeurs d'expression *)

(*Query*)
val sf : projection -> source -> query
val saf : projection -> source -> query 
val sdf : projection -> source -> query
val sfw : projection -> source -> condition -> query 
val safw : projection -> source -> condition -> query
val sdfw : projection -> source -> condition -> query 

(*Column*)
val cexpr : expression -> column 
val cexprAs : expression -> string -> column 

(*COLUMNS*)
val ccol : column -> columns
val ccolplus : column -> columns -> columns 

(*PROJECTION*)
val asterisk : projection 
val projColumn : columns -> projection 

(*EXPRESSION*)
val attribut : string -> expression 
val parE : expression -> expression 
val integer : int -> expression 
val valueFloat : float -> expression 
val minus : expression -> expression -> expression 
val plus : expression -> expression -> expression 
val mult : expression -> expression -> expression 
val div : expression -> expression -> expression 
val minusN : expression -> expression
val chaine : string -> expression 
val string_of_ppipe : expression -> expression -> expression 
val lower : expression -> expression 
val upper : expression -> expression 
val sous_chaine : expression -> expression -> expression -> expression 

(*SOURCE*)
val idS : string -> source
val parS : query -> source
val croissJoin : source -> source -> source
val joinNop : source -> joinNop -> source -> condition -> source 

(*JOINNOP*)
val innerJoin : joinNop 
val join : joinNop 
val outerfull : joinNop 
val outerright : joinNop
val outerleft : joinNop
val left : joinNop 
val right : joinNop 
val full : joinNop 

(*CONDITION*)
val predc : predicate -> condition 
val notC : condition -> condition 
val andC : condition -> condition -> condition 
val orC : condition -> condition -> condition 
val isTrue : condition -> condition 
val isFalse : condition -> condition 
val isUnknown : condition -> condition 
val isNotTrue : condition -> condition 
val isNotFalse : condition -> condition 
val isNotUnknown : condition -> condition 

(*PREDICAT*)
val parP : condition -> predicate
val eq : expression -> expression -> predicate 
val neq : expression -> expression -> predicate 
val lt : expression -> expression -> predicate 
val gt : expression -> expression -> predicate 
val le : expression -> expression -> predicate 
val ge : expression -> expression -> predicate 
val notBtwA : expression -> expression -> expression -> predicate 
val btwA : expression -> expression -> expression -> predicate
val isNull : expression -> predicate 
val isNotNull : expression -> predicate 





(* Conversion en chaîne de caractères pour affichage *)
val string_of_query : query -> string 
val string_of_projection : projection -> string 
val string_of_columns : columns -> string 
val string_of_column : column -> string 
val string_of_expr : expression -> string 
val string_of_source : source -> string 
val string_of_joinop : joinNop -> string 
val string_of_condition : condition -> string 
val string_of_predicat : predicate -> string 


(* Evaluateur *)
val eval_query :
  (R.relation * R.attribute Env.env) Env.env ->
  query -> R.relation * R.attribute Env.env