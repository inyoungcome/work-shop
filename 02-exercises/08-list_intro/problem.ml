open! Base 

let rec length lst = 
  match lst with
  |[] -> 0
  |_ :: t -> 1 + (length t)

let rec sum lst = 
  match lst with
  |[] -> 0
  |hd :: tl -> hd + (sum tl)

let list_append first second = first @ second

let other_list_append first second = (@) first second

let new_head head rest = head :: rest

let%test "Testing sum ...." = 
  Int.(=) 0 (sum [])


let%test "Testing sum ...." = 
  Int.(=) 55 (sum [55])


let%test "Testing sum ...." = 
  Int.(=) 0 (sum [5;-5])


let%test "Testing sum ...." = 
  Int.(=) 11 (sum [5;5;1;1])

let%test "Testing list length...." = 
  Int.(=) 4 (length [1;2;3;4])