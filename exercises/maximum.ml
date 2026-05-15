type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre ;;

let rec maximum a =
  match a with 
  |Vide -> 0 
  |Noeud(x,fg,fd) -> max x (max (maximum fg) (maximum fd))
let res = maximum (Noeud(1,Noeud(3,Vide,Vide),Noeud(5,Vide,Vide)));;
                            