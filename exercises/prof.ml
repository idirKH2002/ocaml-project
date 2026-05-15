type 'a arbre = Vide | Noued of 'a * 'a arbre * 'a arbre ;;


let rec profendeur a =
  match a with 
  | Vide -> 0 
  | Noued(_,fg,fd) -> 1 + max(profendeur fg) (profendeur fd);;

let res = profendeur (Noued(1,Noued(3,Vide,Vide),Noued(5,Vide,Vide)));;