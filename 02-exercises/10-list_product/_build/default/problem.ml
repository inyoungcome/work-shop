open! Base 

let rec product xs = 
  match xs with
  |[] -> 1
  |hd :: tl -> hd * (product tl)


let%test "Testing product..." =
  Int.equal 1 (product [])

let%test "Testing product..." =
  Int.equal 55 (product [55])
