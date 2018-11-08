open! Base 

let number = [1;2;3]

let result =
  List.fold_left  ~init:0  ~f:(fun acc x -> x + acc) number

let () = 
  assert (6 = result )


  module My_list : sig
    val map:('a->'b) ->'a list -> 'b list
    val iter: ('a -> unit) ->'a list ->unit
    val filter:('a -> bool) -> 'a list -> 'a list
  end = struct 
    let map f lst = List.fold_left ~init:[] ~f:(fun acc x -> (f x) :: acc) lst 
    let iter f lst = List.fold_left ~init:() ~f:(fun acc x -> f x) lst
    let filter f lst = List.fold_left ~init:[] ~f:(fun acc x -> if ((f x)) then
    x :: acc
    else acc) lst
  end


  let%test _ =
  Int.equal (List.hd_exn [1;2;3]) 1

(* Similarly, List.tl returns all but the first element of the list. It also raises
   an exception if called on an empty list. The signature is:
   val tl : 'a list -> 'a list
*)
let%test _ =
  [%compare.equal: int list] (List.tl_exn [1;2;3]) [2;3]

(* List.rev returns the reverse of the input list.
   val rev : 'a list -> 'a list
*)
let%test _ =
  [%compare.equal: int list] (List.rev [1;2;3]) [3;2;1]

(* List.mem returns a bool indicating if the given element is contained in the list.
   val mem : 'a list -> equal:('a -> 'a -> bool) -> 'a -> bool
*)

let () = assert (List.mem ~equal:Int.equal [1;2;3] 3)

(* List.sort returns a sorted list in increasing order according to the specified
   comparison function. The comparison function should return a negative number to
   indicate the first element is smaller, 0 to indicate they are equal, and a positive
   number to indicate the first element is larger.
   val sort: compare:('a -> 'a -> int) -> 'a list -> 'a list
*)

(*module My_list : sig
  val map : ('a -> 'b) -> 'a list -> 'b list
  val iter : ('a -> unit) -> 'a list -> unit
  val filter : ('a -> bool) -> 'a list -> 'a list
end = My_list*)

  let%test "Testing My_list.map..." =
    [%compare.equal: int list] [2; 4; 6; 8] (My_list.map (fun x -> 2 * x) [1; 2; 3; 4])

  let%test "Testing My_list.iter..." = 
    let acc = ref 0 in
    My_list.iter (fun x -> if x > !acc then acc := x) [1; 8; 5; 2; 7; 3];
     Int.(=) 8 !acc
