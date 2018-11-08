open! Base 

let is_superman x =
  match x with
  |"Clark Kent" -> true
  | _ -> false

  let non_zero x = 
    match x with
    | 0 -> false
    | _ -> true

let%test "Testing non_zero..." = 
    Bool.(=) false (non_zero 0)

let%test "Testing non_zero..." = 
    Bool.(=) false (non_zero 500)
    
let%test "Testing non_zero..." = 
    Bool.(=) true (non_zero (-500))