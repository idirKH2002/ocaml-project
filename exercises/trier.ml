type 'a arbre = Vide | Noued of 'a * 'a arbre * 'a arbre ;; 

let rec ins x a = 
  match a with 
  |Vide -> Noued (x,Vide,Vide)
  |Noued(y,fg,fd) when x < y -> Noued(y,ins x fg,fd) 
  |Noued(y,fg,fd) -> Noued(y,fg,ins x fd) 

let rec liste_to_arbre l a = 
  match l with 
  |[] -> a
  |x::reste -> liste_to_arbre reste (ins x a) ;;

let rec arbre_to_liste a =
  match a with 
  |Vide->[]
  |Noued(r,fg,fd)->arbre_to_liste fg @ [r] @ arbre_to_liste fd ;;


let trier l = 
  let arbre = liste_to_arbre l Vide in
  arbre_to_liste arbre ;;

let r = trier [1;0;4;2;5;20]