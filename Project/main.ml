type 'a melt = 'a * int ;;
type 'a mset = 'a melt list ;;

let isEmpty l =
  match l with
  | [] -> true
  | _ -> false ;;

let rec cardinal l =
  match l with 
  | [] -> 0 
  | (x,n)::reste -> n + cardinal reste ;;

let rec nbOcc x l =
  match l with
  | [] -> 0
  | (h,n)::reste -> if h = x then n else nbOcc x reste ;;

let rec membre x l =
  match l with
  | [] -> false
  | (h,n)::reste -> if h = x then true else membre x reste ;;

let rec subset l1 l2 = 
  match l1, l2 with
  | [], _ -> true
  | _, [] -> false
  | (h1,n1)::reste1, (h2,n2)::reste2 -> if h1 = h2 then subset reste1 reste2 else false ;;

let rec add x l = 
  match l with
  |[]-> [(x,1)]
  |(h,n)::reste -> if x = h then (h, n + 1)::reste else (h,n)::add x reste ;;

let rec remove (e,n) l =
  match l with
  | [] -> []
  | (h,m) :: reste -> if e = h then 
        let new_m = m - n in
        if new_m <= 0 then reste 
        else (h,new_m)::reste 
      else (h,m)::remove (e,n) reste ;;

let rec equal l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | (h1,m1) :: reste1, (h2,m2) :: reste2 -> 
      if (h1,m1)=(h2,m2) then equal reste1 reste2 else false ;;

let rec sum l1 l2 = 
  match l1,l2 with 
  | [], [] -> [] 
  | [] , _ -> l2
  | _ , [] -> l1 
  | (h1,m1) :: reste1, (h2,m2) :: reste2 -> if (h1=h2) then (h1,m1+m2)::sum reste1 reste2 else (h1,m1) :: (h2,m2)::sum reste1 reste2 ;;

let rec intersection l1 l2 = 
  match l1,l2 with 
  | [], _ | _, [] -> []
  | (h1,m1) :: reste1, (h2,m2) :: reste2 -> 
      if h1 = h2 then (h1,min m1 m2) :: intersection reste1 reste2 
      else intersection reste1 reste2 ;;

let rec difference l1 l2 =
  match l1,l2 with
  | [] , [] -> []
  | [], _ -> l2 
  | _ , [] -> l1
  | (h1,m1) :: reste1, (h2,m2) :: reste2 -> 
      if h1 = h2 then difference reste1 reste2 
      else (h1,m1) :: difference reste1 reste2 ;;

let getRandom (s: 'a mset) : 'a = 
  let total_count = List.fold_left (fun acc (_, count) -> acc + count) 0 s in
  if total_count = 0 then failwith "Ensemble vide"
  else
    let random_index = Random.int total_count in
    let rec find_element currentindex = function
      | [] -> failwith "Ensemble vide"
      | (elem, count) :: rest when currentindex < count -> elem
      | (elem, count) :: rest -> 
          find_element (currentindex - count) rest
    in
    find_element random_index s ;;

type couleur = Rouge | Bleu | Jaune | Noir ;;
type tuile = Joker | Tuile of couleur * int ;;
type main = tuile mset ;;
type pioche = tuile mset ;;
type table = tuile mset ;;
type joueur = {nom : string; main : main; score : int} ;;

type combinaison = 
  | Suite of tuile mset
  | Groupe of tuile mset ;;

let rec combinaisons (l: tuile mset) : tuile mset =
  let rec extraire_suites l acc =
    match l with
    | (Tuile(c1, n1), m1) :: (Tuile(c2, n2), m2) :: reste when c1 = c2 && n2 = n1 + 1 ->
        extraire_suites ((Tuile(c1, n1), m1) :: acc) ((Tuile(c2, n2), m2) :: reste)
    | _ -> if List.length acc >= 3 then List.rev acc else []
  in
  let rec extraire_groupes l acc =
    match l with
    | (Tuile(_, n1), _) :: _ ->
        let groupe, autres = List.partition (fun (t, _) -> match t with
            | Tuile(_, n) -> n = n1
            | _ -> false) l in
        if List.length groupe >= 3 then groupe @ extraire_groupes autres acc
        else extraire_groupes autres acc
    | _ -> acc
  in
  let suites = extraire_suites l [] in
  let groupes = extraire_groupes l [] in
  suites @ groupes ;;

let creer_tuile couleur =
  let rec aux acc i  = 
    if i > 13 then acc 
    else aux ((Tuile(couleur,i),2) :: acc) (i+1)
  in aux [] 1 ;;

let remplire_pioch () = 
  let tuiles_rouges = creer_tuile Rouge in
  let tuiles_bleues = creer_tuile Bleu in
  let tuiles_jaunes = creer_tuile Jaune in
  let tuiles_noires = creer_tuile Noir in
  let jokers = [(Joker, 2)] in
  tuiles_rouges @ tuiles_bleues @ tuiles_jaunes @ tuiles_noires @ jokers ;;

let distribuer_cartes (pioche: pioche) : pioche * main =
  let rec aux acc pioche_rest n =
    if n = 0 then (pioche_rest, acc)
    else
      let carte = getRandom pioche_rest in
      let nouvelle_pioche = remove (carte,1) pioche_rest in
      aux (add carte acc) nouvelle_pioche (n-1)
  in aux [] pioche 14 ;;

let rec sum_tuiles main =
  match main with
  | [] -> 0
  | (Joker,n)::reste -> 20 * n + sum_tuiles reste 
  | (Tuile(_,v),n)::reste -> v * n + sum_tuiles reste ;;

let rec a_combinaison (main: main) : bool =
  let combis = combinaisons main in
  List.length combis > 0 ;;

let table = [] ;;
let pioche = remplire_pioch () ;;

let jouer () = 
  print_string "PREMIER JOUEUR\nEntrez votre nom : ";
  let nom1 = read_line () in 
  let (pioche1, main1) = distribuer_cartes pioche in
  let joueur1 = {nom=nom1; main=main1; score=0} in

  print_string "\nDEUXIEME JOUEUR\nEntrez votre nom : ";
  let nom2 = read_line () in
  let (pioche2, main2) = distribuer_cartes pioche1 in
  let joueur2 = {nom=nom2; main=main2; score=0} in

  let afficher_main joueur =
    print_string ("\nMain de " ^ joueur.nom ^ ":\n");
    List.iter (fun (tuile, nb) ->
        match tuile with
        | Joker -> print_string ("Joker (" ^ string_of_int nb ^ ") ")
        | Tuile(couleur, valeur) -> 
            let couleur_str = match couleur with
              | Rouge -> "Rouge"
              | Bleu -> "Bleu"
              | Jaune -> "Jaune"
              | Noir -> "Noir"
            in
            print_string (couleur_str ^ string_of_int valeur ^ "(" ^ string_of_int nb ^ ") ")
      ) joueur.main;
    print_newline ()
  in

  let rec piocher joueur pioche = 
    let tuile = getRandom pioche in
    let nouvelle_pioche = remove (tuile, 1) pioche in
    let nouvelle_main = add tuile joueur.main in
    print_string "Vous avez pioché : ";
    (match tuile with
     | Joker -> print_string "Joker"
     | Tuile(couleur, valeur) -> 
         let couleur_str = match couleur with
           | Rouge -> "Rouge"
           | Bleu -> "Bleu"
           | Jaune -> "Jaune"
           | Noir -> "Noir"
         in
         print_string (couleur_str ^ string_of_int valeur));
    print_newline ();
    ({joueur with main = nouvelle_main}, nouvelle_pioche)
  in

  let rec poser_tuiles joueur table = 
    afficher_main joueur;
    let combis_posees = combinaisons joueur.main in
    if List.length combis_posees > 0 then (
      print_string "Combinaisons possibles trouvées!\n";
      ({joueur with main = []}, combis_posees @ table)
    ) else (
      print_string "Pas de combinaison possible.\n";
      (joueur, table)
    )
  in

  let rec tour joueur pioche table =
    print_string ("\nTour de " ^ joueur.nom ^ "\n");
    afficher_main joueur;
    print_string "1: Piocher\n2: Poser des tuiles\nVotre choix : ";
    match read_line () with
    | "1" -> piocher joueur pioche
    | "2" -> poser_tuiles joueur table
    | _ -> 
        print_string "\nChoix invalide. Recommencez.\n";
        tour joueur pioche table
  in

  let rec game_loop joueur1 joueur2 pioche table =
    if isEmpty pioche || isEmpty joueur1.main || isEmpty joueur2.main then (
      print_string "\nPartie terminée!\n";
      let score1 = sum_tuiles joueur1.main in
      let score2 = sum_tuiles joueur2.main in
      print_string (joueur1.nom ^ " : " ^ string_of_int score1 ^ " points\n");
      print_string (joueur2.nom ^ " : " ^ string_of_int score2 ^ " points\n");
      if score1 < score2 then print_string (joueur1.nom ^ " gagne!\n")
      else if score2 < score1 then print_string (joueur2.nom ^ " gagne!\n")
      else print_string "Égalité!\n";
      (score1, score2)
    ) else (
      let (joueur1_maj, nouvelle_pioche) = tour joueur1 pioche table in
      let (joueur1_final, nouvelle_table) = poser_tuiles joueur1_maj table in
      game_loop joueur2 joueur1_final nouvelle_pioche nouvelle_table
    )
  in
  game_loop joueur1 joueur2 pioche2 table ;;

(* Pour lancer le jeu *)
let () = 
  Random.self_init ();