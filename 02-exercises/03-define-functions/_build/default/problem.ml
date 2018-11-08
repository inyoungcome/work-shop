open! Base 

let add arg = 
  arg + 1

let string_append  x y = x ^ y

let plus  x y =
  x + y

let times x y = 
  x * y
let minus x y = 
  x - y
let divide x y = 
  x/y

let%test "Testing plus ..." = 
  Int.(=) 2 (plus 1 1)

let%test "Testing plus ..." = 
  Int.equal 49 (plus (-1) 50)

let%test "Testing times..." = 
  Int.(=) 64 (times 8 8)

let%test "Testing divide..." = 
  Int.(=) 512 (divide 1024 2)