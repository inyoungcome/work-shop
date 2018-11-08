open! Base 

type 'a option =
  |None 
  |Some of 'a


let what_number_am_i_thinking (my_number: int option) = 
  match my_number with
  |None -> "I am not thinking of any number!"
  |Some num -> "My number is: " ^ (Int.to_string num)

let%test _=
String.(=) (what_number_am_i_thinking None) "I am not thinking of any number!"

let%test _ =
  String.(=) (what_number_am_i_thinking (Some 7)) "My number is: 7"

let safe_divide ~dividend ~divisor =
  match divisor with
  |0 -> None
  |_ -> Some (dividend / divisor )


let%test "Testing safe_divide..." =
  match (safe_divide ~dividend:3 ~divisor:2) with
  | Some 1 -> true
  | _      -> false

let%test "Testing safe_divide..." =
  match safe_divide ~dividend:3 ~divisor:0 with
  | None -> true
  | _    -> false
