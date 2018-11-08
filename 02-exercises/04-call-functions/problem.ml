open! Base 

let square x = x * x
let half x = x/2
let add x y = x + y

let () = 
  Stdio.printf "(5^2)/2 = %i" (half (square 5))

let () = 
  let squared = square 5 in
  let halfed = half squared in
  Stdio.printf "(5^2)/2 = %i" halfed

let average x y = half (add x y)

let average' x y = let added = add x y in
                      half added 

let%test "Testing average..." = 
  Int.(=) 5 (average 5 5)

let%test "Testing average..." = 
  Int.equal 75 (average  50 100)

