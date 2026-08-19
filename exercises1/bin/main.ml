let double x = x * 2
let () = assert (double 1 = 2)
let () = assert (double 2 = 4)
let () = assert (double 0 = 0)
let () = assert (double (-1) = -2)


let cube x = x *. x *. x
let () = assert (cube 1. = 1.)
let () = assert (cube 2. = 8.)


let signum x = if x < 0
  then -1
  else if x > 0
  then 1
  else 0
let () = assert (signum(-5) = -1)
let () = assert (signum(5) = 1)
let () = assert (signum(0) = 0)


let circle_area r = r *. r *. 22. /. 7.
let () = assert (abs_float(circle_area(7.) -. 154.) < 0.0001)


let rms x y = sqrt((x *. x +. y *. y) /. 2.)
let () = assert (abs_float(rms(3.)(5.) -. 4.1231056) < 0.0001)

let is_valid_date d m = if m = "Feb"
then (d >= 1 && d <= 28)
else if (m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec")
then (d >= 1 && d <= 31)
else (d >= 1 && d <= 30)
let () = assert (is_valid_date 31 "Dec")
let () = assert (not (is_valid_date 31 "Jun"))
let () = assert (is_valid_date 30 "Jun")
let () = assert (not (is_valid_date 30 "Feb"))
let () = assert (is_valid_date 28 "Feb")

let rec fib n = if n = 1
then 1
else if n = 2
then 1
else fib(n-1) + fib(n-2)


let rec h n pp p = if n = 1
then p
else h(n-1)(p)(p+pp)
let rec fib_fast n = h(n)(1)(1)

let (+/.) a b = (a +. b) /. 2.
let () = assert (1.0 +/. 2.0 = 1.5)
let () = assert (0. +/. 0. = 0.)
