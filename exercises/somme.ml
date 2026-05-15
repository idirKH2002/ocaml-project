type 'a arbre = Vide | Noued of 'a * 'a arbre * 'a arbre ;;
let rec somme a = 
  match a with 
  |Vide -> 0 
  |Noued(x,fg,fd) -> x + somme fg + somme fd ;;

let res = somme (Noued(1,Noued(3,Vide,Noued(1,Noued(2,Vide,Vide),Vide)),Noued(5,Vide,Vide)));;