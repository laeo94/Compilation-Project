
let _ =
  (* Chargement des données *)
  let open_source = Lexing.from_channel(open_in "projecttest.txt") in
  let vin_att, vin = Ast.R.from_file "vin.csv" '|' in
  let viticulteur_att, viticulteur = Ast.R.from_file "viticulteur.csv" '|' in
  let client_att, client = Ast.R.from_file "client.csv" '|' in
  let commande_att, commande = Ast.R.from_file "commande.csv" '|' in 

(* Création de l'environnement des relations *)
let env = Env.empty in 
let env = Env.add "vin" (vin, vin_att) env in
let env = Env.add "viticulteur" (viticulteur, viticulteur_att) env in
let env = Env.add "client" (client, client_att) env in
let env = Env.add "commande" (commande, commande_att) env in 

let rec f () = 
  try 
    (* Analyse lexicale et syntaxique d'une requête *)
    let query = Parser.ansyn Lexer.token open_source in

    (* Affichage de la requête *)
    Printf.printf "%s\n" (Ast.string_of_query query); flush stdout;

    (*  Evaluation de la query *)
    let res_query = Ast.eval_query env query in
       (* Inversion des valeur et clé d'attribut dans l'environnement pour l'affichage *)
       match res_query with
       | (r, att_env) -> let rec inverse_string_attribute att_env = 
                            (match att_env with
                            | [] -> []
                            | h :: q -> (match h with
                                        | (s, att) -> (att, s) :: (inverse_string_attribute q)))
                         in
                         let att_env = inverse_string_attribute att_env in
                         Ast.R.print '|' att_env r;
                         Printf.printf "\n\n\n";
      f ()
  with Lexer.Eof -> Printf.printf "End\n"
 in
(* Parsing.set_trace true;*)
  f ()
