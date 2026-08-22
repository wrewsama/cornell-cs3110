module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val of_int : int -> t
end

module type Field = sig
  include Ring
  val ( / ) : t -> t -> t
end

module IntRing : Ring with type t = int = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = ( + )
  let ( ~- ) = ( ~- )
  let ( * ) = ( * )
  let to_string = string_of_int
  let of_int n = n
end

module IntField : Field = struct
  include IntRing
  let ( / ) = ( / )
end

module FloatRing : Ring with type t = float = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = ( +. )
  let ( ~- ) = ( ~-. )
  let ( * ) = ( *. )
  let to_string = string_of_float
  let of_int n = float_of_int n
end

module FloatField : Field = struct
  include FloatRing
  let ( / ) = ( /. )
end

module Rational (M : Ring) = struct
  type t = M.t * M.t
  let zero = (M.zero, M.one)
  let one = (M.one, M.one)
  let ( + ) (a, b) (c, d) = ((a M.( * ) d) M.( + ) (c M.( * ) b), b M.( * ) d)
  let ( ~- ) (a, b) = (M.( ~- )a, b)
  let ( / ) (a, b) (c, d) = (a M.( * ) d, b M.( * ) c)
  let ( * ) (a, b) (c, d) = (a M.( * ) c, b M.( * ) d)
  let to_string (a, b) = M.to_string a ^ "/" ^ M.to_string b
  let of_int n = (n, M.one)
end

module IntRational = Rational(IntRing)
module FloatRational = Rational(FloatRing)
