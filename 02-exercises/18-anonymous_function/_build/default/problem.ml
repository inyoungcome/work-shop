open! Base 

let map_option f opt =
  match opt with
  |None -> None
  |Some x -> Some (f x)

let double  i = 2 * i

let () =
   assert 
   ([%compare.equal: int option]
    (map_option double None )
    None )

let () = 
  assert
  ([%compare.equal: int option]
   (map_option double (Some 2))
   (Some 4))

  let () = 
    assert(
      [%compare.equal : int option]
       (map_option (fun x -> 2*x) (Some 2))
      (Some 4)
    )
  let apply_if_nonzero f i = 
    match i with
    |0 -> 0
    |_ -> f i

  let%test "Testing apply_if_nonzero..." =
    Int.(=) 0 (apply_if_nonzero (fun x -> 10 / x) 0)
  
  let%test "Testing apply_if_nonzero..." =
    Int.(=) 2 (apply_if_nonzero (fun x -> 10 / x) 5)
