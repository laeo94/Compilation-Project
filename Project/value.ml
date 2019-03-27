(* Module pour la représentation et la manipulation des valeurs atomiques
 *
 * Ce module respecte la signature Relation.DATA et peut donc être utilisé
 * en argument de Relation.Make
 *)


(* Définition des types relatant des domaines et des valeurs atomiques manipulables *)

type domain =
  | DInt
  | DFloat
  | DVChar

type value =
  | VInt   of int
  | VFloat of float
  | VVChar of string

(* Fonctions de conversion entre chaînes de caractères et valeurs/domaines (utilisées dans l'import/export des CSV) *)

let domain_of_string s =
  match s with
  | "INT" -> DInt
  | "FLOAT" -> DFloat
  | "VARCHAR" -> DVChar
  | _ -> failwith (Printf.sprintf "Value: domain_of_string: unknown domain: '%s'" s)

let string_of_domain d =
  match d with
  | DInt -> "INT"
  | DFloat -> "FLOAT"
  | DVChar -> "VARCHAR"

let value_of_string d =
  match d with
  | DInt -> (fun s -> VInt (int_of_string s))
  | DFloat -> (fun s -> VFloat (float_of_string s))
  | DVChar -> (fun s -> VVChar s)

let string_of_value v =
  match v with
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VVChar s -> s

(* Fonctions de conversion et de vérification d'appartenance d'une valeur à un domaine *)

let domain_of_value v =
  match v with
  | VInt _ -> DInt
  | VFloat _ -> DFloat
  | VVChar _ -> DVChar

let domain d v =
  match d, v with
  | DInt, VInt _
  | DFloat, VFloat _
  | DVChar, VVChar _ -> true
  | _ -> false

let to_domain d =
  match d with
  | DInt -> (function
    | VInt i -> VInt i
    | VFloat f -> VInt (int_of_float f)
    | VVChar s -> try VInt (int_of_string s) with Failure _ -> VInt 0
  )
  | DFloat -> (function
    | VInt i -> VFloat (float_of_int i)
    | VFloat f -> VFloat f
    | VVChar s -> try VFloat (float_of_string s) with Failure _ -> VFloat 0.
  )
  | DVChar -> (function
    | VInt i -> VVChar (string_of_int i)
    | VFloat f -> VVChar (string_of_float f)
    | VVChar s -> VVChar s
  )

(* Fonction spécifique de manipulation des valeurs (comparaison, addition, concaténation, etc.) *)

let op_arith v1 v2  op1 op2 typeop=
  match (v1,v2) with 
  | VInt i1, VInt i2 -> VInt (op1 i1 i2)
  | VFloat f1, VFloat f2 -> VFloat (op2 f1 f2)
  | VFloat f ,VInt i -> VFloat (op2 f (float_of_int i))
  | VInt i, VFloat f -> VFloat (op2 f (float_of_int i))
  | _ -> failwith (Printf.sprintf "Value: add: type error: '%s '%c' %s'" (string_of_value v1) typeop (string_of_value v2))

let add v1 v2 = op_arith v1 v2 (+) (+.) '+'
let minus v1 v2 = op_arith v1 v2 (-) (-.) '-'
let div v1 v2 = op_arith v1 v2 (/) (/.) '/'
let mul v1 v2= op_arith v1 v2 ( * ) ( *.) '*'

let neg v =
    match v with 
    |VInt i -> VInt (-i)
    |VFloat f -> VFloat (-.f)
    |_-> failwith (Printf.sprintf "Value : neg type error : '%s'" (string_of_value v))
let concat v1 v2 =
  match (v1, v2) with
  | VVChar s1, VVChar s2 -> VVChar (s1 ^ s2)
  | VVChar s1, VInt s2   -> VVChar (s1 ^ (string_of_int s2))
  | VVChar s1, VFloat s2 -> VVChar (s1 ^ (string_of_float s2))
  | _ -> failwith (Printf.sprintf "Value: concat: type error: '%s || %s'" (string_of_value v1) (string_of_value v2))

let op_letter v op typeop =
   match v with
  |VVChar v -> VVChar (op v)
  | _-> failwith (Printf.sprintf "Value : %s : type error : '%s'" typeop (string_of_value v))

let upper v = op_letter v (String.uppercase_ascii) "upper"
let lower v = op_letter v (String.lowercase_ascii) "lower"

let substring v1 v2 v3 =
  match (v1,v2,v3) with
  |VVChar v, VInt i1 ,VInt i2 -> VVChar(String.sub v i2 i2)
  |_-> failwith (Printf.sprintf "Value: substring: type error: '%s' FROM '%s' FOR '%s'" (string_of_value v1) (string_of_value v2) (string_of_value v3))

let op_cond v1 v2 op_int op_float op_s type_op = 
  match(v1,v2)  with
  | VInt vi1,VInt vi2 -> op_int vi1 vi2
  | VInt vi1, VFloat vf1 -> op_float (float_of_int vi1)  vf1
  | VFloat vf1 ,VInt vi1 ->  op_float vf1  (float_of_int vi1) 
  | VFloat vf1, VFloat vf2 -> op_float vf1  vf2
  | VVChar s, VVChar s1 -> op_s s s1 
  |_-> failwith(Printf.sprintf "Value: concat: type error: '%s %s %s'" (string_of_value v1) type_op (string_of_value v2))

let eq  v1 v2 =  op_cond v1 v2 (=)  (=)  (String.equal) "="
let neq v1 v2 =  op_cond v1 v2 (<>) (<>) (fun s1 s2 -> not (String.equal s1 s2)  ) "<>"
let lt  v1 v2 =  op_cond v1 v2 (<)  (<)  (fun s1 s2 -> (String.compare s1 s2) <  0) "<"
let gt  v1 v2 =  op_cond v1 v2 (>)  (>)  (fun s1 s2 -> (String.compare s1 s2) >  0) ">"
let le  v1 v2 =  op_cond v1 v2 (>)  (>)  (fun s1 s2 -> (String.compare s1 s2) <= 0) "<="
let ge  v1 v2 =  op_cond v1 v2 (>)  (>)  (fun s1 s2 -> (String.compare s1 s2) >= 0) ">="

let between v1 v2 v3 =
  match (v1,v2,v3) with
  |VInt i1, VInt i2 , VInt i3 -> (i1>=i2) && (i1<=i3)
  |VFloat f1, VFloat f2 , VFloat f3 -> (f1>=f2) && (f1<=f3)
  |VVChar c1, VVChar c2 , VVChar c3 -> ((String.compare c1 c2) >=0 ) && ((String.compare c1 c3) <=0 )
  |_-> failwith (Printf.sprintf "Value: between : type erro : '%s' BETWEEN '%s' AND '%s" (string_of_value v1) (string_of_value v2) (string_of_value v3))

let notBetween v1 v2 v3 = not (between v1 v2 v3)