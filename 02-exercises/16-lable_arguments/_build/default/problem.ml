open! Base 

let divide dividend divisor  = dividend / divisor

let divide_2 ~dividend ~divisor = dividend / divisor

let modulo ~dividend ~divisor = 
  let x = dividend / divisor in
    dividend - (divisor * x) 

let%test "Testing moduo..." = 
  Int.(=) 3 (modulo ~dividend:15 ~divisor:4)


let%test "Testing moduo..." = 
  Int.(=) 0 (modulo ~dividend:15 ~divisor:5)