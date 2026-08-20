let lst1 = [1;2;3;4;5]
let lst2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
let lst3 = [1] @ [2;3;4] @ [5]

let () = assert (lst1 = lst2)
let () = assert (lst2 = lst3)

let rec product lst =
  match lst with
    | [] -> 1
    | h :: t -> h * product t
let () = assert (product [1;2;3] = 6)


let rec concat lst = 
  match lst with
    | [] -> ""
    | h :: t -> h ^ concat t
let () = assert (concat ["kyun "; "kyun "; "miko "; "kyun"] = "kyun kyun miko kyun")


let pattern1 lst =
  match lst with
    | h :: _ -> h = "bigred"
    | _ -> false

let () = assert (pattern1 ["bigred"; "dog"])
let () = assert (not (pattern1 ["smolpink"; "dog"]))

let pattern2 lst =
  match lst with
  | _ :: _ :: [] | _ :: _ :: _ :: _ :: [] -> true
  | _ -> false
let () = assert (pattern2 ["lorem"; "ipsum"])
let () = assert (not(pattern2 ["lorem"; "ipsum"; "dolor"]))
let () = assert (pattern2 ["lorem"; "ipsum"; "dolor"; "something"])

let pattern3 lst = 
  match lst with
    | a :: b :: _ -> a = b
    | _ -> false
let () = assert (pattern3 ["kyun "; "kyun "; "miko "; "kyun"])
let () = assert (not (pattern3 ["xdd"; "rat"]))


let get5th lst = if List.length lst < 5 then 0 else List.nth lst 4
let () = assert (get5th [6;7] = 0)
let () = assert (get5th [6;9;4;2;3;5] = 3)

let sortdesc lst = List.rev (List.sort Stdlib.compare lst)
let () = assert (sortdesc [6;9;4;2;3;5] = [9;6;5;4;3;2])


let getlast lst = List.nth lst (List.length lst - 1)
let () = assert (getlast [6;7] = 7)
let () = assert (getlast [6;9;4;2;3;5] = 5)

let any_zeroes lst = List.exists (fun x -> x = 0) lst
let () = assert (any_zeroes [1;2;0;5])
let () = assert (not(any_zeroes [1;2;9;5]))


let rec take n lst = if n = 0
then []
else match lst with
| [] -> []
| h :: t -> h :: take (n-1) t
let () = assert (take 2 [1;2;0;5] = [1;2])

let rec drop n lst = if n = 0
then lst
else match lst with
| [] -> []
| h :: t -> drop (n-1) t
let () = assert (drop 2 [1;2;0;5] = [0;5])


let rec take_tail n lst acc = if n = 0
then List.rev acc
else match lst with
| [] -> acc
| h :: t -> take_tail (n-1) t (h :: acc)
let () = assert (take_tail 2 [1;2;0;5] [] = [1;2])


let is_unimodal lst =
  let rec dec lst = match lst with
    | [] -> true
    | h :: [] -> true
    | h :: t -> (h >= List.nth t 0) && dec t
  in 
  let rec uni lst = match lst with
    | [] -> true
    | h :: [] -> true
    | h :: t -> if h > List.nth t 0 then dec t else uni t
  in
  uni lst

let () = assert (is_unimodal [1;3;5;8;4;2])
let () = assert (is_unimodal [1;3;5])
let () = assert (is_unimodal [8;4;2])
let () = assert (not(is_unimodal [1;9;5;8]))

let rec powerset lst = match lst with 
  | [] -> [[]]
  | h :: t -> powerset t @ List.map (fun lst -> h :: lst) (powerset t)

let () = assert (powerset [3] = [[]; [3]])
let () = assert (powerset [3;5] = [[]; [5]; [3]; [3; 5]])


let rec print_int_list lst = match lst with
  | [] -> ()
  | h :: t -> print_int h; print_endline "" ; print_int_list t
let () = print_int_list [6;7;6;9]

let rec print_int_list' lst = List.iter (fun x -> print_int x; print_endline "") lst
let () = print_int_list' [6;7;6;9]


type poketype = Normal | Fire | Water
type pokemon = { name: string; hp: int; ptype: poketype }
let charizard = { name="Charizard"; hp=78; ptype=Fire }
let squirtle = { name="Squirtle"; hp=44; ptype=Water }


let safe_hd lst = match lst with
  | [] -> None
  | h :: _ -> Some h
let () = assert (safe_hd [] = None)
let () = assert (safe_hd [3;5] = Some 3)

let safe_tl lst = match lst with
  | [] -> None
  | _ :: t -> Some t
let () = assert (safe_tl [] = None)
let () = assert (safe_tl [3;5] = Some [5])


let rec max_hp lst = match lst with 
  | [] -> None
  | h :: t -> match max_hp t with
    | None -> Some h
    | Some tailmax -> Some (if h.hp > tailmax.hp then h else tailmax)
let () = assert (max_hp [charizard;squirtle] = Some charizard)


let is_before date1 date2 =
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  if y1 <> y2 
  then y1 < y2
  else if m1 <> m2
  then m1 < m2
  else if d1 <> d2
  then d1 < d2
  else false

let () = assert (is_before (2, 6, 9) (2, 9, 9))
let () = assert (is_before (3, 6, 8) (3, 6, 9))
let () = assert (not(is_before (3, 6, 9) (2, 9, 9)))


let rec earliest lst = match lst with
  | [] -> None
  | h :: t -> match earliest t with
    | None -> Some h
    | Some prev -> Some (if is_before h prev then h else prev)
let () = assert (earliest [(5,1,1); (4,2,3); (3,6,9)] = Some (3,6,9))


let insert k v lst = (k, v) :: lst
let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t
let assoc_lst = insert 1 "one" (insert 2 "two" (insert 3 "three" []))
let () = assert (lookup 2 assoc_lst = Some "two")
let () = assert (lookup 4 assoc_lst = None)


type suit = Diamonds | Clubs | Hearts | Spades
type rank = int
type card = {suit: suit; rank: rank}


type quad = I | II | III | IV
type sign = Neg | Zero | Pos
let sign (x:int) : sign =
  if x < 0
  then Neg
  else if x > 0
  then Pos
  else Zero
let quadrant : int*int -> quad option = fun (x,y) ->
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | _ -> None
let () = assert (quadrant (1, 1) = Some I)
let () = assert (quadrant (-1, 1) = Some II)
let () = assert (quadrant (-1, -1) = Some III)
let () = assert (quadrant (1, -1) = Some IV)
let () = assert (quadrant (0, 67) = None)


let quadrant_when : int*int -> quad option = function
    | (x, y) when (sign x  = Pos && sign y = Pos) -> Some I
    | (x, y) when (sign x  = Neg && sign y = Pos) -> Some II
    | (x, y) when (sign x  = Neg && sign y = Neg) -> Some III
    | (x, y) when (sign x  = Pos && sign y = Neg) -> Some IV
    | _ -> None
let () = assert (quadrant_when (1, 1) = Some I)
let () = assert (quadrant_when (-1, 1) = Some II)
let () = assert (quadrant_when (-1, -1) = Some III)
let () = assert (quadrant_when (1, -1) = Some IV)
let () = assert (quadrant_when (0, 67) = None)

type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree

let rec depth t = match t with 
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (depth l) (depth r)
let () = assert (depth (Node(1, Leaf, Leaf)) = 1)


let rec same_shape t1 t2 = match t1, t2 with
  | Leaf, Leaf -> true
  | (Leaf, Node _) | (Node _, Leaf) -> false
  | Node (v1, l1, r1), Node (v2, l2, r2) -> (v1 = v2) && (same_shape l1 l2) && (same_shape r1 r2)
let () = assert (same_shape (Node(1, Leaf, Leaf)) (Node(1, Leaf, Leaf)))
let () = assert (not (same_shape (Node(1, Leaf, Leaf)) Leaf))


let list_max = function
  | [] -> raise (Failure "empty")
  | h :: t -> List.fold_left max h t
let () = assert (
  try let () = list_max [] in false
  with
  | Failure _ -> true
  | _ -> false
)
let () = assert (list_max[5;1] = 5)


let list_max_string = function
  | [] -> "empty"
  | h :: t -> List.fold_left (fun acc x -> max (acc) (string_of_int x)) (string_of_int h) t
let () = assert (list_max_string[] = "empty")
let () = assert (list_max_string[5;1] = "5")


let is_bst t =
  let rec check t = match t with
    | Leaf -> (true, max_int, min_int)
    | Node (v, l, r) -> 
      let (bst1, min1, max1) = check l in
      let (bst2, min2, max2) = check r in
      (bst1 && bst2 && (max1 <= v) && (v <= min2), min min1 v, max max2 v)
  in
  match check t with
    | (bst, _, _) -> bst

let () = assert (is_bst (Node(1, Node(0, Leaf, Leaf), Node(67, Leaf, Leaf))))
let () = assert (not(is_bst (Node(100, Node(0, Leaf, Leaf), Node(67, Leaf, Leaf)))))



let sign2 (x) =
  if x < 0
  then `Neg
  else if x > 0
  then `Pos
  else `Zero
let quadrant2 = fun (x,y) ->
  match sign2 x, sign2 y with
    | `Pos, `Pos -> Some `I
    | `Neg, `Pos -> Some `II
    | `Neg, `Neg -> Some `III
    | `Pos, `Neg -> Some `IV
    | _ -> None
let () = assert (quadrant2 (1, 1) = Some `I)
let () = assert (quadrant2 (-1, 1) = Some `II)
let () = assert (quadrant2 (-1, -1) = Some `III)
let () = assert (quadrant2 (1, -1) = Some `IV)
let () = assert (quadrant2 (0, 67) = None)
