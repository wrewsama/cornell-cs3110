module type ComplexSig = sig
  type t = float * float
  val zero : t
  val add : t -> t -> t
end


module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end


type ('k, 'v) tree = 
  | Leaf
  | Node of 'k * 'v * ('k, 'v) tree * ('k, 'v) tree
module type Map = sig
  type ('k, 'v) t
  val empty  : ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val lookup : 'k -> ('k, 'v) t -> 'v
end
module BstMap : Map = struct
  type ('k, 'v) t = ('k, 'v) tree
  exception NotFound
  let empty = Leaf
  let insert k v m =
    let rec dfs k v m = match m with
      | Leaf -> Node (k, v, Leaf, Leaf)
      | Node (k', v', l, r) -> if k <= k'
        then Node (k', v', (dfs k v l), r)
        else Node (k', v', l, (dfs k v r))
    in
    dfs k v m

  let lookup k m =
    let rec dfs k m = match m with
      | Leaf -> raise NotFound
      | Node (k', v, l, r) -> if k == k'
        then v
        else if k < k'
        then dfs k l
        else dfs k r
    in
    dfs k m
end
let () = assert (BstMap.(empty |> insert 1 "one" |> insert 2 "two" |> lookup 2) = "two")


module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  (** [make n d] represents n/d, a fraction with 
      numerator [n] and denominator [d].
      Requires d <> 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end
module FractionImpl : Fraction = struct
  type t = int * int
  let make n d = (n, d)
  let numerator f = fst f
  let denominator f = snd f
  let to_string (n, d) = (string_of_int n) ^ "/" ^ (string_of_int d)
  let to_float (n, d) = (float_of_int n) /. (float_of_int d)

  let add (n1, d1) (n2, d2) = 
    let d = d1 * d2 in
    let n = n1 * d2 + n2 * d1 in
    (n, d)
  let mul (n1, d1) (n2, d2) = 
    let d = d1 * d2 in
    let n = n1 * n2 in
    (n, d)
end
let () =
  let f1 = FractionImpl.make 3 5 in
  let f2 = FractionImpl.make 6 7 in
  assert (FractionImpl.((add f1 f2) |> to_string) = "51/35")
let () =
  let f1 = FractionImpl.make 3 5 in
  let f2 = FractionImpl.make 6 7 in
  assert (FractionImpl.((mul f1 f2) |> to_string) = "18/35")


module ReducedFractionImpl : Fraction = struct
  type t = int * int
  let rec gcd x y =
    if x = 0 then y
    else if (x < y) then gcd (y - x) x
    else gcd y (x - y)
  let simplify (n, d) =
    let divisor = gcd n d in
    (n / divisor, d / divisor) 

  let make n d = simplify(n, d)
  let numerator f = fst f
  let denominator f = snd f
  let to_string (n, d) = (string_of_int n) ^ "/" ^ (string_of_int d)
  let to_float (n, d) = (float_of_int n) /. (float_of_int d)

  let add (n1, d1) (n2, d2) = 
    let d = d1 * d2 in
    let n = n1 * d2 + n2 * d1 in
    simplify(n, d)
  let mul (n1, d1) (n2, d2) = 
    let d = d1 * d2 in
    let n = n1 * n2 in
    simplify(n, d)
end
let () = assert (ReducedFractionImpl.(make 6 8 |> to_string) = "3/4")
let () =
  let f1 = ReducedFractionImpl.make 1 2 in
  let f2 = ReducedFractionImpl.make 2 3 in
  assert (ReducedFractionImpl.((mul f1 f2) |> to_string) = "1/3")
let () =
  let f1 = ReducedFractionImpl.make 1 2 in
  let f2 = ReducedFractionImpl.make 1 2 in
  assert (ReducedFractionImpl.((add f1 f2) |> to_string) = "1/1")


module CharMap = Map.Make(Char)
let m = CharMap.(empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra" |> add 'V' "Victor")
let () = assert (CharMap.find 'E' m = "Echo")
let m2 = CharMap.remove 'A' m
let () = assert (not (CharMap.mem 'A' m2))


type date = {month : int; day : int}
module Date = struct
  type t = date
  let compare d1 d2 = if d1.month < d2.month
    then -1
    else if d1.month > d2.month
    then 1
    else if d1.day < d2.day
    then -1
    else if d1.day > d2.day
    then 1
    else 0
end
let () = assert (Date.compare {month=3; day=5} {month=2; day=28} = 1)
let () = assert (Date.compare {month=3; day=5} {month=4; day=1} = -1)
let () = assert (Date.compare {month=3; day=5} {month=3; day=5} = 0)


module DateMap = Map.Make(Date)
type calendar = string DateMap.t
let my_calendar : calendar = DateMap.(empty |> add {month=3; day=5} "birthday" |> add {month=8; day=1} "debut anniversary")


let print_calendar calendar =
  DateMap.iter (fun date event -> Printf.printf "%d/%d: %s\n" date.month date.day event) calendar
let () = print_calendar my_calendar 


let is_for (m : string CharMap.t): string CharMap.t = CharMap.mapi (fun k v -> Printf.sprintf "%c is for %s" k v) m
let () =
  let m = CharMap.(empty |> add 'a' "aa" |> add 'b' "bb") in
  let m2 = is_for m in
  assert (CharMap.find 'a' m2 = "a is for aa");
  assert (CharMap.find 'b' m2 = "b is for bb")


let first_after (cal: calendar) (start_date: Date.t) = snd (DateMap.find_first (fun d -> Date.compare d start_date = 1) cal)
let () = assert (first_after my_calendar {month=6; day=9} = "debut anniversary")
let () = assert (first_after my_calendar {month=1; day=9} = "birthday")



module CaseInsensitiveString = struct
  type t = string
  let compare s1 s2 = String.compare (String.lowercase_ascii(s1)) (String.lowercase_ascii(s2))
end
let () = assert (CaseInsensitiveString.compare "xdd" "xDd" = 0)
module CaseInsensitiveStringSet = Set.Make(CaseInsensitiveString)

let () =
  let s1 = CaseInsensitiveStringSet.(empty |> add "xdd" |> add "ppx") in
  let s2 = CaseInsensitiveStringSet.add "XDD" s1 in
  assert (CaseInsensitiveStringSet.equal s1 s2)


module type ToString = sig
  type t
  val to_string : t -> string
end


module Print (M : ToString) = struct
  let print (v: M.t): unit = print_endline (M.to_string v)
end


module Int: ToString with type t = int = struct
  type t = int
  let to_string = string_of_int
end
module PrintInt = Print(Int)
let () = PrintInt.print(35)



module MyString: ToString with type t = string = struct
  type t = string
  let to_string x = x
end
module PrintString = Print(MyString)
let () = PrintString.print("miko")


module StringWithPrint = struct
  include String
  include PrintString
end
let () = StringWithPrint.("suisei" |> uppercase_ascii |> print)

