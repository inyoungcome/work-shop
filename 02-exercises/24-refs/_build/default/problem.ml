open! Base

let x = ref 0

let () = 
  x:= !x + 1

let default x v = match v with
|None -> x
|Some y -> y



let min_and_max list =
   let min = ref (default 0 (List.hd list)) in
   let max = ref (default 0 (List.hd list)) in
   List.iter list ~f:(fun x -> match x with 
                                   |x when (x < !min) -> min := x
                                  |x when (x > !max) -> max := x 
                                   |_ -> ());
  (!min,!max)

  let%test "Testing min_and_max..." =
  [%compare.equal: int*int] (min_and_max [5;9;2;4;3]) (2,9) 
;;

let%test "Testing min_and_max..." =
  [%compare.equal: int*int] (min_and_max [1;3;4;4;11;15;7;34]) (7,34)
;;

let%test "Testing min_and_max..." =
  [%compare.equal: int*int] (min_and_max [11;11;11;11;11]) (11,11)
;;
                                        
