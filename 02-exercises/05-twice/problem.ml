open! Base 

let add1 x = x + 1

let square x = x * x

let twice f x = f (f x)

let add2 x = twice add1 x

let raise_to_the_fourth  x = twice square x

let%test "Testing add1..." = 
  Int.(=) 5 (add1 4)

let%test "Testing square..." = 
  Int.(=) 25 (square 5)

let%test "Testing square..." = 
  Int.(=) 25 (square (-5))

let%test "Testing twice add1..." = 
  Int.(=) 5 (twice add1 3)

let%test "Testing twice add1..." = 
  Int.(=) 1334 (twice add1 1333)

let%test "Testing twice add2..." = 
  Int.(=) 1335 (add2 1333)

let%test "Testing twice raise_to_the_fourth..." = 
  Int.(=) 10000 (raise_to_the_fourth 10)
