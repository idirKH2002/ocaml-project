type 'a arbre = Vide | Noued of 'a * 'a arbre * 'a arbre ;; 

let rec ins x a = 
  match a with 
  |Vide -> Noued (x,Vide,Vide)
  |Noued(y,fg,fd) when x < y -> Noued(y,ins x fg,fd) 
  |Noued(y,fg,fd) -> Noued(y,fg,ins x fd) 




let res = ins 10 (Noued(1,Noued(3,Vide,Vide),Noued(5,Vide,Vide)));;
                            