let test_isEmpty () =
  assert (isEmpty [] = true);
  assert (isEmpty [(1,2)] = false);
  print_endline "Tests validé ";;


let test_cardinal () =
  assert (cardinal [] = 0);
  assert (cardinal [(1,2); (2,3)] = 5);
  assert (cardinal [(1,1)] = 1);
  print_endline "Tests validé ";;


let test_nbOcc () =
  let l = [(1,2); (2,3); (3,1)] in
  assert (nbOcc 1 l = 2);
  assert (nbOcc 2 l = 3);
  assert (nbOcc 4 l = 0);
  print_endline "Tests validé ";;


let test_membre () =
  let l = [(1,2); (2,3); (3,1)] in
  assert (membre 1 l = true);
  assert (membre 4 l = false);
  print_endline "Tests validé ";;


let test_subset () =
  let l1 = [(1,2); (2,3)] in
  let l2 = [(1,2); (2,3); (3,1)] in
  assert (subset l1 l1 = true);
  assert (subset [] l1 = true);
  assert (subset l1 l2 = false);
  print_endline "Tests validé ";;


let test_add () =
  assert (add 1 [] = [(1,1)]);
  assert (add 1 [(1,1)] = [(1,2)]);
  assert (add 2 [(1,1)] = [(1,1); (2,1)]);
  print_endline "Tests validé ";;


let test_remove () =
  let l = [(1,2); (2,3); (3,1)] in
  assert (remove (1,1) l = [(1,1); (2,3); (3,1)]);
  assert (remove (2,3) l = [(1,2); (2,0); (3,1)]);
  print_endline "Tests validé ";;


let test_equal () =
  let l1 = [(1,2); (2,3)] in
  let l2 = [(1,2); (2,3)] in
  let l3 = [(1,2); (2,2)] in
  assert (equal l1 l2 = true);
  assert (equal l1 l3 = false);
  assert (equal [] [] = true);
  print_endline "Tests validé ";;


let test_sum () =
  let l1 = [(1,2); (2,3)] in
  let l2 = [(2,1); (3,1)] in
  assert (sum l1 l2 = [(1,2); (2,4); (3,1)]);
  assert (sum [] l1 = l1);
  assert (sum l1 [] = l1);
  print_endline "Tests validé ";;


let test_intersection () =
  let l1 = [(1,2); (2,3)] in
  let l2 = [(2,1); (3,1)] in
  assert (intersection l1 l2 = [(2,1)]);
  assert (intersection [] l1 = []);
  assert (intersection l1 [] = []);
  print_endline "Tests validé ";;


let test_difference () =
  let l1 = [(1,2); (2,3)] in
  let l2 = [(2,1); (3,1)] in
  assert (difference l1 l2 = [(1,2)]);
  assert (difference [] l1 = l1);
  assert (difference l1 [] = []);
  print_endline "Tests validé ";;



let test_creer_tuile () =
  let tuiles = creer_tuile Rouge in
  assert (List.length tuiles = 13);
  assert (List.for_all (fun (t,n) -> n = 2) tuiles);
  print_endline "Tests validé ";;


let test_remplire_pioch () =
  let pioche = remplire_pioch() in
  assert (List.length pioche = 53); 
  assert (cardinal pioche = 106); 
  print_endline "Tests validé ";;


let test_combinaison () =
  let l = [(Tuile(Rouge,1),1); (Tuile(Rouge,2),1)] in
  let result = combinaison l in
  assert (List.length result = 1);
  assert (match List.hd result with
      | Suite(Rouge, [1;2]) -> true
      | _ -> false);
  print_endline "Tests validé ";;


test_isEmpty();
test_cardinal();
test_nbOcc();
test_membre();
test_subset();
test_add();
test_remove();
test_equal();
test_sum();
test_intersection();
test_difference();
test_creer_tuile();
test_remplire_pioch();
test_combinaison();