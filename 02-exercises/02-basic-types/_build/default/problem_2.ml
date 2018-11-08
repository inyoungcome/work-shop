open! Base

let four = 4
let float_four = 4.

(*
let int_average x y = failwith "For you to implement"
*)
(*
let int_average x y = 
(x + y ) /2
*)
let int_average = fun x y -> (x+y)/2
(*
let float_average x y = failwith "For you to complement"
*)
let float_average x y =
(x +. y) /. 2.

let first_name = "yang"
let last_name:string = "chunquan"

let full_name = first_name ^ " " ^last_name

let a_boolean_false:bool = false

let () = assert (true || a_boolean_false)

let () = 
  Stdio.print_endline "Hi ,My name is ";
  Stdio.print_endline full_name;
  Stdio.print_endline "and I am 5 years old"

let () = 
  Stdio.printf "Hi, My name is %s and I am %d years old" full_name 5

let%test "Testing in_average..." = 
  Int.equal (int_average 5 5) 5

let%test "Testing int_average..." = 
  Int.equal (int_average 50 70) 50

let%test "Testing float_average..." = 
  Float.(=) (float_average 5. 5.) 5.