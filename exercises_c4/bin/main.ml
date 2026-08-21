let rec repeat f n x = if n = 0
  then x
  else f (repeat f (n-1) x)

let () = assert (repeat (fun x -> x * 2) 0 1 = 1)
let () = assert (repeat (fun x -> x * 2) 1 1 = 2)
let () = assert (repeat (fun x -> x * 2) 3 1 = 8)


let product_left lst = List.fold_left ( *. ) 1. lst
let product_right lst = List.fold_right ( *. ) lst 1.

let () = assert (product_left [1.;2.;3.] = 6.)
let () = assert (product_right [1.;2.;3.] = 6.)

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j
let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> (x mod 2 = 1))
  |> List.map (fun x -> x * x)
  |> List.fold_left ( + ) 0
let () = assert (sum_cube_odd 3 = 10)
let () = assert (sum_cube_odd 5 = 35)


let rec exists_rec p lst = match lst with
  | [] -> false
  | h :: t -> (p h) || (exists_rec p t)
let exists_fold p lst = List.fold_left (fun acc x -> (p x) || acc) false lst
let exists_lib = List.exists
let () = assert (exists_rec (fun x -> (x mod 2 = 0)) [1;3;5;8])
let () = assert (exists_fold (fun x -> (x mod 2 = 0)) [1;3;5;8])
let () = assert (exists_lib (fun x -> (x mod 2 = 0)) [1;3;5;8])
let () = assert (not(exists_rec (fun x -> (x mod 2 = 0)) [1;3;5]))
let () = assert (not(exists_fold (fun x -> (x mod 2 = 0)) [1;3;5]))
let () = assert (not(exists_lib (fun x -> (x mod 2 = 0)) [1;3;5]))


let rec acc_balance_rec = function
  | [] -> 0
  | h :: t -> (acc_balance_rec t) - h
let acc_balance_l lst = -1 * (List.fold_left ( + ) 0 lst)
let acc_balance_r lst = -1 * (List.fold_right ( + ) lst 0)
let () = assert (acc_balance_rec [1;2;3] = -6)
let () = assert (acc_balance_l [1;2;3] = -6)
let () = assert (acc_balance_r [1;2;3] = -6)


let uncurried_append (l1, l2) = List.append l1 l2
let uncurried_compare (c1, c2) = Char.compare c1 c2
let uncurried_max (x, y) = Stdlib.max x y
let () = assert (uncurried_append ([1;2], [3;4]) = [1;2;3;4])
let () = assert (uncurried_compare ('a', 'b') = -1)
let () = assert (uncurried_max (6, 7) = 7)



let () = 
  let f x = x + 1 in
  let g x = x * 2 in
  let lst = [1;2;3] in
assert (List.map (fun x -> f (g x)) lst = List.map f (List.map g lst))


let filter_len_lt3 lst = List.filter (fun s -> (String.length s) > 3) lst
let () = assert (filter_len_lt3 ["miko"; "chi"; "x"; "suisei"] = ["miko"; "suisei"])
let add_1_float lst = List.map (fun x -> x +. 1.) lst
let () = assert (add_1_float [1.;2.] = [2.; 3.])
let temu_join strs sep = List.fold_right (fun x acc -> if acc = "" then x else x ^ sep ^ acc) strs ""
let () = assert (temu_join ["hi"; "bye"] "," = "hi,bye")


let uniq_keys alst = 
  alst
  |> List.map (fun (k, _) -> k)
  |> List.sort_uniq compare
  |> List.length
let () = assert (uniq_keys [(1, 1); (2, 1); (1, 1)] = 2)


let is_valid_matrix m = m
  |> List.map List.length
  |> List.sort_uniq compare
  |> List.length
  = 1
let () = assert (is_valid_matrix [[1; 1; 1]; [9; 8; 7]])
let () = assert (not(is_valid_matrix []))
let () = assert (not(is_valid_matrix [[1; 2]; [3]]))


let row_vec_add v1 v2 = List.map2 (fun x1 x2 -> x1 + x2) v1 v2
let () = assert (row_vec_add [1; 1; 1] [9; 8; 7] = [10;9;8])


let matrix_add m1 m2 = List.map2 row_vec_add m1 m2
let () = assert (matrix_add [[1;1]; [1;1]] [[1;2]; [3;4]] = [[2;3]; [4;5]])


let dot_prod v1 v2 = List.map2 ( * ) v1 v2 |> List.fold_left ( + ) 0
let () = assert (dot_prod [1;2;3] [4;5;6] = 32)
let transpose m = match m with
  | [] -> failwith "invalid mat"
  | first_row :: _ -> 0 -- ((List.length first_row) - 1)
    |> List.map (fun colnum -> List.map (fun row -> List.nth row colnum) m)
let () = assert (transpose [[1;2;3];[4;5;6]] = [[1;4];[2;5];[3;6]])
let multiply_matrices m1 m2 =
  let m2t = transpose m2 in
  List.map (fun r1 -> List.map (fun r2 -> dot_prod r1 r2) m2t) m1
let () = assert (multiply_matrices [[1;2];[3;4]] [[6;7];[8;9]] = [[22;25]; [50;57]])
