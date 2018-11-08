open! Base 

let plus x y = x + y
let times x y = x * y

let rec add_every_number_up_to x =
  match x with
  |0 -> 0
  |_ -> plus x (add_every_number_up_to (x-1))

let rec up_to answer combine x = 
  match x with
  |0 -> answer
  |_ -> combine x (up_to answer combine (x-1))

let simple_add_every_number_up_to x = 
  up_to 0 plus x

let simple_factorial x =
  up_to 1 times x

let rec sum xs = 
  match xs with
  |[] -> 0
  |x :: ys -> plus x (sum ys)

let rec product xs =
  match xs with
  |[] -> 1
  |x ::ys -> times x (product ys)


let rec every answer combine xs =
  match xs with
  |[] -> answer
  |x :: ys -> combine x (every answer combine ys) 

let simpler_sum xs = 
  every 0 plus xs

let simpler_product xs = 
  every 1 times xs

let%test "Testing simpler_product..." =
  Int.(=) 1 (simpler_product [])

let%test "Testing simpler_product..." =
  Int.(=) 55 (simpler_product [55])

let%test "Testing simpler_product..." =
  Int.(=) 25 (simpler_product [5; (-5) ; 1 ; (-1)])

let%test "Testing simpler_product..." =
  Int.(=) 25 (simpler_product [5 ; 5 ; 1 ; 1])

let%test "Testing simpler_sum..." =
  Int.(=) 0 (simpler_sum [])

  let%test "Testing simpler_sum..." =
  Int.(=) 55 (simpler_sum [55])

let%test "Testing simpler_sum..." =
  Int.(=) 0 (simpler_sum [5 ; (-5) ; 1 ; (-1)])

let%test "Testing simpler_sum..." =
  Int.(=) 12 (simpler_sum [5 ; 5 ; 1 ; 1])