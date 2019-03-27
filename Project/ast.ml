
open Value 
open Relation 
open Env

module R = Relation.Make(Value)

(*(* Syntaxe abstraite *)*)
type query = 
	| SF of  projection * source
	| SAF of projection * source
	| SDF of projection * source
	| SFW of  projection * source * condition
	| SAFW of projection * source * condition 
	| SDFW of projection * source * condition

and column =
	| Cexpr of expression 
	| CexprAs of expression * string

and columns = 
	| Ccol of column
	| CcolPlus of column * columns

and projection =
	| AsterixP
	| ProjColumn of columns

and  expression = 
	| Attribut of string 
	| ParE of expression
	| Integer of int 
	| Float of float
	| Minus of expression * expression
	| Plus of expression * expression
	| AsterixE of expression * expression
	| Slash of expression * expression
	| MinusN of expression
	| String of string
	| String_of_ppipe of  expression * expression
	| Lower of expression
	| Upper of expression
	| Substring of expression * expression* expression

and joinNop =
	| INNERJOIN
	| JOIN
	| OUTERLEFT
	| OUTERRIGHT
	| OUTERFULL
	| LEFTJOIN
	| RIGHTJOIN
	| FULLJOIN

and source =
	| IdS of string
	| ParS of query
	| CrossJoin of source * source
	| JoinNop of source * joinNop * source * condition



and  condition = 
	| PredC of predicate
	| Not of condition
	| And of condition * condition
	| Or of condition * condition
	| IsTrue of condition
	| IsFalse of condition
	| IsUnknown of condition
	| IsNotTrue of condition
	| IsNotFalse of condition
	| IsNotUnknown of condition

and predicate =
 	| ParP of condition
 	| EQ  of expression * expression
 	| NEQ of expression * expression
 	| LT of expression * expression
 	| GT of expression * expression
 	| LE of expression * expression
 	| GE of expression * expression
 	| NotBetweenAnd of expression * expression * expression
 	| BetweenAnd of expression * expression * expression
 	| IsNULL of expression
 	| IsNotNull of expression

 (* Constructeurs des type *)

(*QUERY*)
let sf pro s = SF (pro,s);;
let saf pro s = SAF (pro,s);;
let sdf pro s = SDF (pro,s) ;;
let sfw pro s cond = SFW (pro,s,cond);;
let safw pro s cond = SAFW (pro,s,cond);;
let sdfw pro s cond = SDFW (pro,s,cond) ;;

(*COLUMN*)
let cexpr expr = Cexpr(expr);;
let cexprAs expr s = CexprAs(expr,s);;

(*COLUMNS*)
let ccol col =  Ccol (col);;
let ccolplus col1 col2 = CcolPlus(col1,col2);;

(*PROJECTION*)
let asterisk = AsterixP;;
let projColumn col = ProjColumn (col);;

(*EXPRESSION*)
let attribut s1  = Attribut(s1);;
let parE expr = ParE (expr);;
let integer v = Integer(v);;
let valueFloat v = Float(v);;		
let minus e1 e2 = Minus(e1,e2);;
let plus e1 e2 = Plus(e1,e2);;
let mult e1 e2 = AsterixE(e1,e2);;
let div e1 e2 = Slash(e1,e2);;
let minusN e1 = MinusN(e1);;
let chaine s = String(s);;
let string_of_ppipe e1 e2 = String_of_ppipe(e1,e2);;
let lower e = Lower(e);;
let upper e = Upper(e);;
let sous_chaine e1 e2 e3 = Substring(e1,e2,e3);;

(*Source*)
let idS s = IdS(s);;
let parS e =  ParS (e);;
let croissJoin src des = CrossJoin(src,des);;
let joinNop src join des cond = JoinNop(src,join,des,cond);;

(*Join*)
let innerJoin = INNERJOIN;;
let join = JOIN;;
let outerfull = OUTERFULL;;
let outerright = OUTERRIGHT;;
let outerleft = OUTERLEFT;;
let left = LEFTJOIN;;
let right = RIGHTJOIN;;
let full = FULLJOIN;;

(*Condition*)
let predc pre = PredC(pre);;
let notC cond =  Not(cond);;
let andC cond1 cond2 = And(cond1,cond2);;
let orC cond1 cond2 = Or (cond1,cond2);;
let isTrue cond = IsTrue (cond);;
let isFalse cond  = IsFalse(cond);;
let isUnknown cond = IsUnknown(cond);;
let isNotTrue cond = IsNotTrue (cond);;
let isNotFalse cond  = IsFalse(cond);;
let isNotUnknown cond = IsNotUnknown(cond);;

(* PREDICATE *)
let parP cond = ParP(cond);;
let eq e1 e2 = EQ(e1,e2);;
let neq e1 e2 = NEQ (e1,e2);;
let lt e1 e2 = LT(e1,e2);;
let gt e1 e2 = GT(e1,e2);;
let le e1 e2 = LE(e1,e2);;
let ge e1 e2 = GE (e1,e2);;
let notBtwA e1 e2 e3 = NotBetweenAnd(e1,e2,e3);;
let btwA e1 e2 e3 = BetweenAnd(e1,e2,e3);;
let isNull e = IsNULL(e);;
let isNotNull e = IsNotNull(e);;


(**********************Pretty Printer****************)

(*Fonction qui permet d'afficher le query*)
let rec string_of_query query = 
	match query with
	| SF (p,s) -> 
		Printf.sprintf "SELECT %s FROM %s" (string_of_projection p) (string_of_source s)
	| SAF (p,s) -> 
		Printf.sprintf "SELECT ALL %s FROM %s" (string_of_projection p) (string_of_source s)
	| SDF (p,s) -> 
		Printf.sprintf "SELECT DISTINCT %s FROM %s" (string_of_projection p) (string_of_source s)
	| SFW (p,s,c) -> 
		Printf.sprintf "SELECT %s FROM %s WHERE %s" (string_of_projection p) (string_of_source s) (string_of_condition c)
	| SAFW (p,s,c) -> 
		Printf.sprintf "SELECT ALL %s FROM %s WHERE %s" (string_of_projection p) (string_of_source s) (string_of_condition c)
	| SDFW (p,s,c) -> 
		Printf.sprintf "SELECT DISTINCT %s FROM %s WHERE %s" (string_of_projection p) (string_of_source s) (string_of_condition c)

(*Fonction qui permet d'afficher la projection*)
and string_of_projection projection =
		match projection with
		| AsterixP -> 
			Printf.sprintf "*"
		| ProjColumn c -> 
			Printf.sprintf "%s" (string_of_columns c)

(*Fonction qui permet d'afficher plusieurs colonnes*)
and string_of_columns columns = 
		match columns with
		| Ccol c -> 
			Printf.sprintf "%s" (string_of_column c)
		| CcolPlus (c1,c2) ->
			Printf.sprintf "%s , %s " (string_of_column c1) (string_of_columns c2)

(*Fonction qui permet d'afficher la colonne*)
and string_of_column column =
		match column with
		| Cexpr e -> 
			Printf.sprintf "%s" (string_of_expr e) 
		| CexprAs (e,id) -> 
			Printf.sprintf "%s AS %s" (string_of_expr e) id

(*Fonction qui permet d'afficher l' expression*)
and string_of_expr expr = 
		match expr with
		| Integer i ->
			Printf.sprintf "%d" i
		| Float f -> 
			Printf.sprintf "%f" f
		| String s -> 
			Printf.sprintf "%s" s
		| Attribut (s) -> 
			Printf.sprintf "%s" s 
		| ParE e -> 
			Printf.sprintf "(%s)" (string_of_expr e)
		| Minus (e1,e2) -> 
			Printf.sprintf "%s-%s" (string_of_expr e1) (string_of_expr e2)
		| Plus (e1,e2) -> 
			Printf.sprintf "%s+%s" (string_of_expr e1) (string_of_expr e2)
		| AsterixE (e1,e2) -> 
			Printf.sprintf "%s*%s" (string_of_expr e1) (string_of_expr e2)
		| Slash (e1,e2) -> 
			Printf.sprintf "%s/%s" (string_of_expr e1) (string_of_expr e2)
		| MinusN e -> 
			Printf.sprintf "-%s" (string_of_expr e)
		| String_of_ppipe (e1,e2) ->
			Printf.sprintf "%s||%s" (string_of_expr e1) (string_of_expr e2)
		| Lower e ->
			Printf.sprintf "(%s)"(String.lowercase_ascii (string_of_expr e))
		| Upper e -> 
			Printf.sprintf "(%s)"(String.uppercase_ascii (string_of_expr e))
		| Substring (e1,e2,e3) -> 
			Printf.sprintf "(%s FROM %s FOR %s)"(string_of_expr e1) (string_of_expr e2) (string_of_expr e3)

(*Fonction qui permet d'afficher la source*)
and string_of_source source = 
		match source with
		| IdS s ->
			Printf.sprintf "%s" s
		| ParS e -> 
			Printf.sprintf "(%s)" (string_of_query e)
		| CrossJoin (s1,s2) ->
			Printf.sprintf "%s CROISS JOIN %s "(string_of_source s1) (string_of_source s2)
		| JoinNop (s1,j,s2,c) ->
			Printf.sprintf "%s %s %s ON %s" (string_of_source s1) (string_of_joinop j) (string_of_source s2) (string_of_condition c)

(*Fonction qui permet d'afficher le joinop*)
and string_of_joinop join = 
		match join with 
		| INNERJOIN ->
			 "INNER JOIN"
		| JOIN -> 
			"JOIN"
		| OUTERLEFT -> 
			"LEFT OUTER JOIN"
		| OUTERRIGHT -> 
			"RIGHT OUTER JOIN"
		| OUTERFULL -> 
			"FULL OUTER JOIN"
		| LEFTJOIN -> 
			"LEFT JOIN"
		| RIGHTJOIN -> 
			"RIGHT JOIN"
		| FULLJOIN -> 
			"FULL JOIN"

(*Fonction qui permet d'afficher la condtion *)
and string_of_condition cond = 
		match cond with
		| PredC p -> 
			Printf.sprintf "%s" (string_of_predicat p)
		| Not c -> 
			Printf.sprintf "NOT %s" (string_of_condition c)
		| And (c1,c2) ->
			Printf.sprintf "%s AND %s" (string_of_condition c1) (string_of_condition c2)
		| Or (c1,c2) -> 
			Printf.sprintf "%s OR %s" (string_of_condition c1) (string_of_condition c2)
		| IsTrue c -> 
			Printf.sprintf "%s IS TRUE" (string_of_condition c)
		| IsFalse  c -> 
			Printf.sprintf "%s IS FALSE" (string_of_condition c)
		| IsUnknown  c -> 
			Printf.sprintf "%s IS UNKNOWN" (string_of_condition c)
		| IsNotTrue  c -> 
			Printf.sprintf "%s IS NOT TRUE" (string_of_condition c)
		| IsNotFalse  c -> 
			Printf.sprintf "%s IS NOT FALSE" (string_of_condition c)
		| IsNotUnknown c -> 
			Printf.sprintf "%s IS NOT UNKNOWN" (string_of_condition c)

(*Fonction qui permet d'afficher le predicat*)
and string_of_predicat pred = 
		match pred with
		| ParP c -> 
			Printf.sprintf "(%s)" (string_of_condition c)
 		| EQ  (e1,e2) -> 
 			Printf.sprintf "%s = %s" (string_of_expr e1) (string_of_expr e2)
 		| NEQ (e1,e2) -> 
 			Printf.sprintf "%s <> %s" (string_of_expr e1) (string_of_expr e2)
 		| LT (e1,e2) -> 
 			Printf.sprintf "%s < %s" (string_of_expr e1) (string_of_expr e2)
 		| GT (e1,e2) -> 
 			Printf.sprintf "%s > %s" (string_of_expr e1) (string_of_expr e2)
 		| LE (e1,e2) -> 
 			Printf.sprintf "%s <= %s" (string_of_expr e1) (string_of_expr e2)
 		| GE (e1,e2) ->
 			Printf.sprintf "%s >= %s" (string_of_expr e1) (string_of_expr e2)
 		| NotBetweenAnd (e1,e2,e3) ->
 			Printf.sprintf "%s NOT BETWEEN %s AND %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
 		| BetweenAnd (e1,e2,e3) -> 
 			Printf.sprintf "%s BETWEEN %s AND %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
 		| IsNULL e -> 
 			Printf.sprintf "%s IS NULL" (string_of_expr e)
 		| IsNotNull e ->
 			Printf.sprintf "%s IS NOT NULL" (string_of_expr e)

(***********************************************************************************EVALUATEUR***********************************************************************************************************)

let rec eval_expr env e = 
	(* Application de l'opération value sur une expression *)
	let calcul_exprSolo e1 t value =
		let expr_e1 = (eval_expr env e1) t in
			begin 
				match expr_e1 with
				| Some s -> value s
				|_ -> failwith "error"
			end
	in		
	(* Application de l'opération value sur deux expressions *)
	let calcul_exprDuo e1 e2 t value = 
		let expr_e1 = (eval_expr env e1) t in 
		let expr_e2 = (eval_expr env e2) t in
			begin
				match expr_e1,expr_e2 with 
				| Some e, Some e3 -> value e e3
				|_ -> failwith "error"
			end 
	in
	(* Application de l'opération value sur trois expressions *)
	let calcul_exprTriple e1 e2 e3 t value = 
			let expr_e1 = (eval_expr env e1) t in 
			let expr_e2 = (eval_expr env e2) t in
			let expr_e3 = ( eval_expr env e3) t in 
			 	begin 
			 		match expr_e1,expr_e2, expr_e3 with 
					| Some e, Some e3,Some e4 -> value e e3 e4
					|_ -> failwith "error"
				end
	in
		match e with 
		| Attribut(s) ->    (* Renvoie le value option associe au l'attribut tuple *)
			begin 
				(fun t ->	match (Env.find s env) with 
							| Some att -> R.attribute att t
							| None -> failwith "error ide" )
			end
		(*Les autres cas prends le tuples et renvoie le value option *)
		| Integer e -> 
			(fun t -> Some (VInt e))
		| Float f -> 
			(fun t -> Some(VFloat f)) 
		| String s -> 
			(fun t -> Some(VVChar s))
		| ParE e ->  
			(eval_expr env e)
		| Minus (e1,e2) -> 
			(fun t -> Some(calcul_exprDuo e1 e2 t Value.minus))
		| Plus (e1,e2) -> 
			(fun t -> Some(calcul_exprDuo e1 e2 t Value.add))
		| AsterixE (e1,e2) -> 
			(fun t-> Some(calcul_exprDuo e1 e2 t Value.mul))
		| Slash (e1,e2) -> 
			(fun t -> Some(calcul_exprDuo e1 e2 t Value.div ))
		| MinusN e -> 
			(fun t-> Some( calcul_exprSolo e t Value.neg ))
		| String_of_ppipe (s,s2) -> 
			(fun  t -> Some(calcul_exprDuo s s2 t Value.concat))
		| Lower e -> 
			(fun t ->Some (calcul_exprSolo e t Value.lower))
		| Upper e -> 
			(fun t -> Some(calcul_exprSolo e t Value.upper))
		| Substring (s,s1,s2) -> 
			(fun t -> Some(calcul_exprTriple s s1 s2 t Value.substring))

(*Meme schema que le eval_expression sauf que ici cela renvoie un boolean de l'evaluation du tuple *)
and eval_cond env cond = 
	match cond with
	| PredC p ->  
		eval_pred env p
	| Not c -> 
		(fun t -> not ((eval_cond env c) t))
	| And (c1,c2) ->
		(fun t -> ((eval_cond env c1)t) && ((eval_cond env c2)t))
	| Or (c1,c2) -> 
		(fun t -> ((eval_cond env c1)t) || ((eval_cond env c2)t))
	| IsTrue c -> 
		(fun t -> ((eval_cond env c)t))
	| IsFalse c -> 
		(fun t -> ((eval_cond env c) t))
	| IsUnknown c -> 
		(fun t -> ((eval_cond env c)t))
	| IsNotTrue c -> 
		(fun t -> not ((eval_cond env c)t))
	| IsNotFalse c -> 
		(fun t -> not ((eval_cond env c)t))
	| IsNotUnknown c -> 
		(fun t -> not ((eval_cond env c)t))

and eval_pred env pred =
	(* Application de l'opération value sur deux epressions *)
	let predDuo e1 e2 t value = 
	 	let pred_cond1 = ( eval_expr env e1 ) t in
	 	let pred_cond2 =  ( eval_expr env e2 ) t in
		 	(match pred_cond1, pred_cond2 with 
		 		|Some e , Some e3 -> value e e3
		 		|_ -> failwith "error")
	 in
	 (* Application de l'opération value sur deux epressions *)
	 let predTriple e1 e2 e3 t value =
		let pred_cond1 = ( eval_expr env e1 ) t in
	 	let pred_cond2 = ( eval_expr env e2 ) t in
	 	let pred_cond3 = ( eval_expr env e3 ) t in 
	 		( match pred_cond1, pred_cond2,pred_cond3 with 
		 		|Some e , Some e3, Some e4 -> value e e3 e4 
		 		|_ -> failwith "error")
	 in
	(*Renvoie la valeur boolean du tuple*)
	 match pred with
	| ParP c ->  
		(eval_cond env c)
	| EQ (e1,e2) ->
		(fun t ->  predDuo e1 e2 t Value.eq )
	| NEQ (e1,e2) ->
		(fun t ->  predDuo e1 e2 t Value.neq )
	| LT (e1,e2) ->	
		(fun t ->  predDuo e1 e2 t Value.lt ) 
	| GT (e1,e2) -> 
		(fun t ->  predDuo e1 e2 t Value.gt )
	| LE (e1,e2) -> 
		(fun t ->  predDuo e1 e2 t Value.le )
	| GE (e1,e2) -> 
		(fun t ->  predDuo e1 e2 t Value.ge )
	| NotBetweenAnd (e1,e2,e3) -> 
		(fun t ->  predTriple e1 e2 e3 t Value.notBetween )
	| BetweenAnd (e1,e2,e3) -> 
		(fun t ->  predTriple e1 e2 e3 t Value.between )
	| _-> failwith (Printf.sprintf "Error")

and eval_source env source =
	(* Fusionne les deux environnements d'attribut *)
	let fusion_env env1 env2 l : R.attribute Env.env =
		begin
			let rec loop env1 env2 =
				(match env2 with
				|[] -> env1
				|(k,v)::t -> loop(Env.add k (v+l) env1)t
				)
			in loop env1 env2
		end
	in
	(* 
	* Recherche la source et renvoie celle ci, dans le cas des jointures,
	* on calcul chaque valeur des sources et faisons la jointures des ces derniers
	* ainsi que la fusion de leurs environnement 
	* exemple FROM a, b, c, d -> CrossJoin(a, CrossJoin(b, CrossJoin(c, d)))
	*)
	match source with
	| IdS s ->
		begin
			match (Env.find s env) with
				|Some (s) ->  s
				|_-> failwith (Printf.sprintf "Error")
		end
	| ParS q ->
		eval_query env q 
	| CrossJoin (s1,s2) ->
		let source_s1, env1 = (eval_source env s1) in
		let source_s2, env2 = (eval_source env s2) in
		let lpos = R.width source_s1 in
		(R.crossjoin source_s1 source_s2), (fusion_env env1 env2 lpos)
	|JoinNop (s1,join,s2,c) ->
		match join with 
		| INNERJOIN 
		| JOIN ->
			let source_s1, env1 = (eval_source env s1)  in
			let source_s2, env2 = (eval_source env s2)  in
			let lpos = R.width source_s1 in
			let env12 = (fusion_env env1 env2 lpos) in
			let pred2t = 
				fun t1 t2 -> 
					let pred = (eval_cond env12 c) in
					pred t1 || pred t2
			in
			(R.innerjoin pred2t source_s1 source_s2), env12
		| OUTERLEFT
		| LEFTJOIN -> 
		let source_s1, env1 = (eval_source env s1)  in
			let source_s2, env2 = (eval_source env s2)  in
			let lpos = R.width source_s1 in
			let env12 = (fusion_env env1 env2 lpos) in
			let pred2t = 
				fun t1 t2 -> 
					let pred = (eval_cond env12 c) in
					pred t1 || pred t2
			in
			(R.leftouterjoin pred2t source_s1 source_s2), env12
		| OUTERRIGHT 
		| RIGHTJOIN -> 
			let source_s1, env1 = (eval_source env s1)  in
			let source_s2, env2 = (eval_source env s2)  in
			let lpos = R.width source_s1 in
			let env12 = (fusion_env env1 env2 lpos) in
			let pred2t = 
				fun t1 t2 -> 
					let pred = (eval_cond env12 c) in
					pred t1 || pred t2
			in
			(R.leftouterjoin pred2t source_s1 source_s2), env12
		| OUTERFULL
		| FULLJOIN -> 		
			let source_s1, env1 = (eval_source env s1)  in
			let source_s2, env2 = (eval_source env s2)  in
			let lpos = R.width source_s1 in
			let env12 = (fusion_env env1 env2 lpos) in
			let pred2t = 
				fun t1 t2 -> 
					let pred = (eval_cond env12 c) in
					pred t1 || pred t2
			in
			(R.fullouterjoin pred2t source_s1 source_s2), env12

(* Evaluation de la projection besoin d'evaluer les columns.
 * eval colunmn prend l'environnement,la colonne, le resultat & un compteur n 
 * Le résultat est accumulé dans la variable res 
 *)
and eval_column env col res n =
	(* g est l'envi gamma de l'attribut, la list associé aux couple domain, fonction *)
	let g, l = res in
	match col with
	| Cexpr e -> 
		(g, l @ [DVChar, eval_expr env e])
	| CexprAs (e,s) -> 
		((Env.add s n g), l @ [DVChar, eval_expr env e])

(* fonction recursive primitive *)
and eval_columns env lcol res n =
	match lcol with 
	| [] -> res
	| h::t ->
		eval_columns env t (eval_column env h res n) (n + 1) 

and eval_projection env projection =
	(* fonction converti un object columns en liste de colones *)
	let rec col_to_list c =
		match c with
		| Ccol c -> [c]
		| CcolPlus (c, cs) -> c :: (col_to_list cs) 
	in
	
	match projection with
	| AsterixP -> (Env.empty, [])
	| ProjColumn c -> 
		eval_columns env (col_to_list c) (Env.empty, []) 0 

and eval_query env query =
	match query with 
	| SF (p,s) ->
		(*Si p est * alors on va renvoie la source *)
		begin
			match p with 
			| AsterixP ->
				eval_source env s
			| _ ->
				(* si p est une liste de colonnes bien precise alors on va recuperer la 
				 *l'eval source sachant que c un couple et on va faire une projection
				 * en prend en compte projection p et lenvironnement gamma qui va donne 
				 *un nouvelle environnement et sa projection
				 *)
				let r, g = eval_source env s in
				let g', proj = eval_projection g p in
				(R.projection proj r, g')
		end
	| SAF (p,s) -> 
				begin

					match p with 
					|AsterixP -> 
						eval_source env s
					| _ ->
						let r, g = eval_source env s in
						let g', proj = eval_projection g p in
						(R.projection proj r, g')
				 end 
	| SDF (p,s) -> 
				begin
					(*Dans ce cas on distinct les doublons*)
					match p with 
					|AsterixP -> 
							let r,g = eval_source env s in
							let g', proj = eval_projection g p in 
							let r = R.distinct(R.projection proj r) in
							r, g'
					| _ ->
						let r,g = eval_source env s in 
						let g', proj = eval_projection g p in 
						let r = R.distinct(R.projection proj r) in
						r, g'
				end			
	| SFW (p,s,c) -> 
				begin
					(* evalue la source sachant qu'une selection a besoin 
				     *d'un predicat de r et environnement 
				     *)
					 match p with 
					|AsterixP -> let r,g = eval_source env s in
						let pred = (eval_cond g c) in
						(R.selection pred r , g)
					| _ ->
					(*on ajoute eval_projection pour projeter la colonne qui nous interresse*)
						let r, g = eval_source env s in
						let pred = eval_cond g c in
						let r = R.selection pred r in
						let g', proj = eval_projection g p in
						(R.projection proj r, g')
				end
	| SAFW (p,s,c) ->
				begin
					 match p with 
					|AsterixP -> let r,g = eval_source env s in
						let pred = eval_cond g c in
						(R.selection pred r , g)
					| _ ->
						let r, g = eval_source env s in
						let pred = eval_cond g c in
						let r = R.selection pred r in
						let g', proj = eval_projection g p in
						(R.projection proj r, g')
				end

	| SDFW (p,s,c) -> 
				begin
					match p with 
					|AsterixP -> 
						let r,g = eval_source env s in
						let pred = eval_cond g c in
						(R.selection pred r , g)
					|_-> 		
						let r,g = eval_source env s in
						let pred = eval_cond g c in
						let r = R.selection pred r in
						let g', proj = eval_projection g p in 
						let r = R.distinct(R.projection proj r) in
						r, g'
					end

