open! Base 

let rec every answer combine xs =
  match xs with
  |[] -> answer
  |x :: ys -> combine x (every answer combine ys)

let rec largest xs = 
  match xs with
  |[] -> Float.neg_infinity
  |x :: ys -> Float.max x (largest xs)

let rec smallest xs = 
  match xs with
  |[] -> Float.infinity
  |x :: ys -> Float.min x (smallest ys)

let simpler_largest xs =
  every Float.neg_infinity Float.max xs

let simpler_smallest xs = 
  every Float.infinity Float.min xs


 let%test "Testing simpler_smallest..." =
  Float.(=) Float.infinity (simpler_smallest [])

let%test "Testing simpler_smallest..." =
  Float.(=) 55. (simpler_smallest [55.])

let%test "Testing simpler_smallest..." =
  Float.(=) (-5.) (simpler_smallest [5. ;(-5.) ; 1. ;(-1.)])

let%test "Testing simpler_smallest..." =
  Float.(=) 1. (simpler_smallest [5. ; 5. ; 1. ; 1.])

let%test "Testing simpler_largest..." =
  Float.(=) Float.neg_infinity (simpler_largest [])

let%test "Testing simpler_largest..." =
  Float.(=) 55. (simpler_largest [55.])

  let%test "Testing simpler_largest..." =
  Float.(=) (5.) (simpler_largest [5. ; (-5.) ; 1. ; (-1.)])

let%test "Testing simpler_largest..." =
  Float.(=) 5. (simpler_largest [5. ; 5. ; 1. ; 1.])