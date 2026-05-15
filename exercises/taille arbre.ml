type 'a arbre = Vide |Noued of 'a * 'a arbre * 'a arbre ;;

let rec taille a = 
  match a with 
  |Vide -> 0 
  |Noued(_,fg,fd) -> 1 + taille fg + taille fd ;;

let a = Noued(1,Noued(3,Vide,Vide),Noued(5,Vide,Vide));;
let k = taille a ;;