open! Base 

let  rec add_every_number_up_to x = 
  assert (x >= 0);
  match x with
  |0 -> 0
  |_ -> x + (add_every_number_up_to (x-1))

let rec factorial x = 
  assert (x>=0);
  match x with
  |0 -> 1
  |_ ->  x * factorial (x-1)

let%test "Testing factorial..." = 
  Int.(=) 1 (factorial 0)

let%test "Testing factorial..." = 
  Int.(=) 1 (factorial 1)

let%test "Testing factorial..." = 
  Int.(=) 24 (factorial 4)
  
let%test "Testing factorial..." = 
  Int.(=) 12 (factorial 5)

let%test "Testing factorial..." =
  Int.(=) (479001600) (factorial 12)