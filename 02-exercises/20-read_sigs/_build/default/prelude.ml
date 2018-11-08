module Alist  = struct 
  type t = int list
  let of_list x = x
  let empty = []
  let filter f x  = List.filter  f x 
  let number_from_0_to_50 =
    let rec compute x acc = 
      if x = 0 then 0 :: acc
      else compute (x-1) (x::acc)
      in 
      compute 50 []
    end

    let number_from_0_to_50 = 
      let rec compute x acc = 
        if x = 0 then 0::acc
        else compute (x-1) (x::acc)
        in 
        compute 50 []
        ;;

let evens_from_0_to_50' : Alist.t  = List.filter (fun x -> x mod 2 =0 ) number_from_0_to_50;;

let odds_from_0_to_50'  : Alist.t  = List.filter (fun x -> x mod 2 =1 ) number_from_0_to_50;;

let number_from_0_to_10' :Alist.t = List.filter (fun x -> x <= 10) number_from_0_to_50;;