open! Base 

let () = 
  assert ([%compare.equal: int list]
          (5 :: [1;2;5;8])
          [5;1;2;5;8])

let () = 
  assert ([%compare.equal: int list] ([1;2] @ [5;8]) [1;2;5;8]);
  assert ([%compare.equal: int list] (List.append [1;2] [5;8]) [1;2;5;8])

let rec range from to_ = 
  assert (from <= to_);
  let interval = to_ - from in
  match interval with
  |0 -> []
  |_ -> from :: (range (from+1) (to_))

let%test "Testing range ..." = 
  [%compare.equal: int list] (range 1 4) [1;2;3]

let%test "Testing range..." =
  [%compare.equal: int list] (range (-5) 3) [-5;-4;-3;-2;-1;0;1;2]